-- | Static analysis passes for OpenQASM 3 compilation.

module LinguaQuanta.Qasm.Passes
  ( AbstractionErr(..)
  , InversionErr(..)
  , elimInv
  , elimPow
  , toAst
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.AST (AstStmt(..))
import LinguaQuanta.Qasm.Expression
  ( ExprErr
  , toArrayIndex
  , toConstInt
  , toRValue
  , toVoidCall
  )
import LinguaQuanta.Qasm.Inversion (invertGate)
import LinguaQuanta.Qasm.Gate
  ( Gate(NamedGate)
  , GateSummaryErr
  , exprToGate
  , validateGate
  )
import LinguaQuanta.Qasm.GateName (GateName(UserDefined))
import LinguaQuanta.Qasm.Language
  ( Expr(..)
  , GateExpr
  , LValue(..)
  , Stmt(..)
  , Type(..)
  )

-------------------------------------------------------------------------------
-- * Pass Template.

-- | A "per-line pass" is a translation pass that rewrites each statement of an
-- AST independently. A per-line pass is defined by a function f that consumes
-- a line number and program statement (of AST type a), and returns either a
-- list of program statements (each of AST type b) or an error (of type c). The
-- applyPerLinePass function consumes such an f, and returns an analysis pass
-- that applies f to each statement of an AST.
applyPerLinePass :: (Int -> a -> Either [b] c) -> Int -> [a] -> Either [b] c
applyPerLinePass _ _ []           = Left []
applyPerLinePass f n (line:lines) =
    case f n line of
        Left stmt -> case applyPerLinePass f (n + 1) lines of
            Left rest -> Left (stmt ++ rest)
            Right err -> Right err
        Right err -> Right err

-- | An "safe per-line pass" is equivalent to a per-line pass, except that the
-- error case is never encountered.
applySafePerLinePass :: (a -> [b]) -> [a] -> [b]
applySafePerLinePass _ []           = []
applySafePerLinePass f (line:lines) = f line ++ applySafePerLinePass f lines

-------------------------------------------------------------------------------
-- * Primary Pass: Abstraction Pass.

data AbstractionErr = GateAbstractionErr Int GateSummaryErr
                    | ArrayLenAbstractionErr Int ExprErr
                    | RValueAbstractionErr Int ExprErr
                    | VoidCallAbstractionErr Int ExprErr
                    | UnknownExprStmt Int
                    | NonPosArrayLen Int
                    deriving (Show, Eq)

type AbstractionRes = Either [AstStmt] AbstractionErr

-- | Consumes a line number (ln) and an expression (expr). If expr evaluates to
-- a valid gate (g) that is supported by the translator, then g is returned.
-- Otherwise, an error is returned with line number set to ln.
abstractQasmGate :: Int -> GateExpr -> AbstractionRes
abstractQasmGate ln expr =
    case exprToGate expr of
        Left (n, gate) -> case validateGate gate of
            Nothing  -> Left [AstGateStmt n gate]
            Just err -> Right (GateAbstractionErr ln err)
        Right err -> Right (GateAbstractionErr ln err)

-- | Consumes a line number (ln) and the lenght of an array given as an
-- OpenQASM exprsesion (expr). If the array length is valid, then its integer
-- representation is returned. If the array length is invalid, then an error is
-- returned with line number set to ln.
getDeclLen :: Int -> Expr -> Either Int AbstractionErr
getDeclLen ln expr =
    case toConstInt expr of
        Left n -> if n > 0
                  then Left n
                  else Right $ NonPosArrayLen ln
        Right err -> Right $ ArrayLenAbstractionErr ln err

-- | Consumes a line number (ln), a variable type, and the name of the declared
-- variable (decl). If the declaration if valid, then an abstract declaration
-- for a variable with name decl is returned. The constructor of the abstract
-- declaration is determined by the type. If the type is invalid, then an error
-- is returned with line number set to ln.
abstractQasmDecl :: Int -> Type -> String -> AbstractionRes
abstractQasmDecl _  QubitT           decl = Left [AstQubitDecl Nothing decl]
abstractQasmDecl ln (QubitArrT expr) decl =
    case getDeclLen ln expr of
        Left n    -> Left [AstQubitDecl (Just n) decl]
        Right err -> Right err
abstractQasmDecl _  BitT decl = Left [AstBitDecl Nothing decl]
abstractQasmDecl ln (BitArrT expr) decl =
    case getDeclLen ln expr of
        Left n    -> Left [AstBitDecl (Just n) decl]
        Right err -> Right err

-- | Implementation details for abstractQasmAssign.
abstractQasmAssignImpl :: Int -> String -> Maybe Int -> Expr -> AbstractionRes
abstractQasmAssignImpl ln id idx expr = 
    case toRValue expr of
        Left rval -> Left [AstAssign id idx rval]
        Right err -> Right $ RValueAbstractionErr ln err

-- | Consumes a line number (ln), a variable to update, and a value to assign
-- to the variable (rval). If the lvalue and rvalue are valid, then an abstract
-- assignment statement for rval into lval is returned. Otherwise, an error for
-- the first failure is returned. is returned.
abstractQasmAssign :: Int -> LValue -> Expr -> AbstractionRes
abstractQasmAssign ln (CVar id) expr = stmt
    where stmt = abstractQasmAssignImpl ln id Nothing expr
abstractQasmAssign ln (CReg id idx) expr =
    case toArrayIndex idx of
        Left n    -> abstractQasmAssignImpl ln id (Just n) expr
        Right err -> Right $ ArrayLenAbstractionErr ln err

-- | Consumes a line number (ln), a variable type, the name of a newly declared
-- variable, and a value to which the variable is initialized (rval). If the
-- declaration and initial value are valid, then an abstract declaration
-- statement for a variable of the name decl, an an abstract initialization
-- statement setting decl to expr, are returned. Otherwise, an error for the
-- first failure is returned. is returned.
abstractQasmInitDecl :: Int -> Type -> String -> Expr -> AbstractionRes
abstractQasmInitDecl ln ty decl rval =
    case abstractQasmDecl ln ty decl of
        Left dstmt -> case abstractQasmAssign ln (CVar decl) rval of
            Left istmt -> Left $ dstmt ++ istmt
            Right err  -> Right err
        Right err -> Right err

-- | Consumes a line number (ln) and an expression (expr) intended to act as a
-- statement. If expr evaluates to a valid expression statement, then the
-- corresponding abstract statement is returned. Otherwise, an error for the
-- first failure is returned. is returned.
abstractQasmExprStmt :: Int -> Expr -> AbstractionRes
abstractQasmExprStmt ln (Brack expr) = astmts
    where astmts = abstractQasmExprStmt ln expr
abstractQasmExprStmt ln (Call name args) =
    case toVoidCall name args of
        Left call -> Left [AstCall call]
        Right err -> Right $ VoidCallAbstractionErr ln err
abstractQasmExprStmt ln _ = Right $ UnknownExprStmt ln

-- | Converts a single statement into a sequence of equivalent AST statements.
-- If then conversion fails, then an appropriate abstraction error is returned.
abstractStmt :: Int -> Stmt -> AbstractionRes
abstractStmt ln (QasmGateStmt expr) = astmts
    where astmts = abstractQasmGate ln expr
abstractStmt ln (QasmDeclStmt ty decl) = astmts
    where astmts = abstractQasmDecl ln ty decl
abstractStmt ln (QasmAssignStmt lval rval) = astmts
    where astmts = abstractQasmAssign ln lval rval
abstractStmt ln (QasmInitDeclStmt ty decl rval) = astmts
    where astmts = abstractQasmInitDecl ln ty decl rval
abstractStmt ln (QasmExprStmt expr) = astmts
    where astmts = abstractQasmExprStmt ln expr

-- | Converts a list of statements into an AST. If the conversion fails, then
-- an appropriate abstraction error is returned.
toAst :: [Stmt] -> AbstractionRes
toAst = applyPerLinePass abstractStmt 1

-------------------------------------------------------------------------------
-- * Secondary Pass: Inverse Elimination

data InversionErr = UnknownUserDefinedInv Int String
                  | UnknownNativeInv Int GateName
                  | UnhandledInvErr Int
                  deriving (Show, Eq)

-- | Inlines the inverse circuit (circ) to a gate g, where g is repeated n
-- times. If circ contains a single gate inv, then pow(n) @ inv is returned.
-- Otherwise, circ is promoted to sequence of (AstGateStmt 0) statements,
-- with the sequence repeated min(1, n) times.
inlineInv :: Int -> [Gate] -> [AstStmt]
inlineInv n [inv] = [AstGateStmt n inv]
inlineInv 0 circ  = map (AstGateStmt 0) circ
inlineInv 1 circ  = inlineInv 0 circ
inlineInv n circ  = concat $ replicate n $ inlineInv 0 circ

-- | If invertGate is unable to invert a gate g, then either g is user-defined,
-- or the inverse of g is unknown. If g is user-defined, and an inverse can be
-- resolved, then the inverse is returned. Otherwise, an appropriate error is
-- returned.
resolveUnknownInv :: Int -> Gate -> Either [AstStmt] InversionErr
resolveUnknownInv ln (NamedGate (UserDefined str) _ _ _) = Right err
    where err = UnknownUserDefinedInv ln str
resolveUnknownInv ln (NamedGate name _ _ _) = Right err
    where err = UnknownNativeInv ln name
resolveUnknownInv ln _ = Right err
    where err = UnhandledInvErr ln

-- | Consumes a single AST statement. If the statement is an inverted gate,
-- then the inverse circuit is returned. Otherwise, the statement is returned
-- unchanged. If inlining fails, then an appropriate error is returned.
elimInvImpl :: Int -> AstStmt -> Either [AstStmt] InversionErr
elimInvImpl ln (AstGateStmt n gate) =
    case invertGate gate of
        Just inv -> Left (inlineInv n inv)
        Nothing  -> resolveUnknownInv ln gate
elimInvImpl _ stmt = Left [stmt]

-- | Inlines all inverse gates in a list of AST statements. If inlining fails,
-- then an appropriate error is returned.
elimInv :: [AstStmt] -> Either [AstStmt] InversionErr
elimInv = applyPerLinePass elimInvImpl 1

-------------------------------------------------------------------------------
-- * Secondary Pass: Integral Power Elimination

-- | Inlines the power modifers of a single AST statements.
elimPowImpl :: AstStmt -> [AstStmt]
elimPowImpl (AstGateStmt 0 gate) = [AstGateStmt 0 gate]
elimPowImpl (AstGateStmt n gate) = replicate n $ AstGateStmt 0 gate
elimPowImpl stmt                 = [stmt]

-- | Inlines all power modifers in a list of AST statements.
elimPow :: [AstStmt] -> [AstStmt]
elimPow = applySafePerLinePass elimPowImpl
