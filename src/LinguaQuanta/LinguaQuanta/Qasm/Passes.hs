-- | Static analysis passes for OpenQASM 3 compilation.

module LinguaQuanta.Qasm.Passes
  ( AbstractionErr(..)
  , InlineErr(..)
  , InversionErr(..)
  , ToLscErr(..)
  , elimFun
  , elimInv
  , elimPow
  , toAst
  , toLsc
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.AST (AstStmt(..))
import LinguaQuanta.Qasm.Call
  ( elimCallsInArglist
  , elimCallsInExpr
  )
import LinguaQuanta.Qasm.Expression
  ( ExprErr
  , parseGateOperand
  , toArrayIndex
  , toConstInt
  , toRValue
  , toVoidCall
  )
import LinguaQuanta.Qasm.Inversion (invertGate)
import LinguaQuanta.Qasm.Gate
  ( Gate(..)
  , GateSummaryErr
  , exprToGate
  , nullGateMod
  , validateGate
  )
import LinguaQuanta.Qasm.GateName
  ( GateName(..)
  , isBackportGate
  , isQelib1Gate
  , isQuipperGate
  )
import LinguaQuanta.Qasm.Header
  ( QasmHeader
  , isLegacy
  , usingBkp
  , usingQfn
  , usingQpr
  , usingStd
  )
import LinguaQuanta.Qasm.Language
  ( Expr(..)
  , GateExpr
  , GateOperand
  , LValue(..)
  , Stmt(..)
  , Type(..)
  )
import LinguaQuanta.Qasm.LatticeSurgery
  ( LscGateErr(..)
  , lscRewriteGate
  )
import LinguaQuanta.Qasm.Operand
  ( RValue(..)
  , VoidCall(..)
  )

-------------------------------------------------------------------------------
-- * Pass Template.

-- | A function that takes a line number and input line, and computes either a
-- sequence of output lines or an error.
type PerLineFn ins outs err = Int -> ins -> Either [outs] err

-- | Specialization of PerLineFn to functions which output AST statements.
type PerStmtFn ins err = PerLineFn ins AstStmt err

-- | Extends PerLineFn to retain state across function calls.
type StatefulFn st ins outs err = Int -> st -> ins -> Either (st, [outs]) err

-- | A "stateful pass" is a pure-line pass the retains state across each call.
-- The input and output of the PerLineFn are extended to tuples, where the
-- first component is the program state, and the second component is the input
-- (resp. output) data.
applyStatefulPass :: StatefulFn s a b c -> Int -> s -> [a] -> Either (s, [b]) c
applyStatefulPass _ _ st []           = Left (st, [])
applyStatefulPass f n st (line:lines) =
    case f n st line of
        Left (st', stmt) -> case applyStatefulPass f (n + 1) st' lines of
            Left (st'', rest) -> Left (st'', stmt ++ rest)
            Right err         -> Right err
        Right err -> Right err

-- | A "per-line pass" is a translation pass that rewrites each statement of an
-- AST independently. A per-line pass is defined by a function f that takes as
-- input a line number and program statement (of AST type a), and returns
-- either a list of program statements (each of AST type b) or an error (of
-- type c). The PerLineFn consumes such an f, and returns an analysis pass that
-- applies f to each statement of an AST.
applyPerLineFn :: PerLineFn a b c -> Int -> [a] -> Either [b] c
applyPerLineFn _ _ []           = Left []
applyPerLineFn f n (line:lines) =
    case f n line of
        Left stmt -> case applyPerLineFn f (n + 1) lines of
            Left rest -> Left (stmt ++ rest)
            Right err -> Right err
        Right err -> Right err

-- | An "safe per-line pass" is equivalent to a per-line pass, except that the
-- error case is never encountered.
applySafePerLineFn :: (a -> [b]) -> [a] -> [b]
applySafePerLineFn _ []           = []
applySafePerLineFn f (line:lines) = f line ++ applySafePerLineFn f lines

-------------------------------------------------------------------------------
-- * Primary Pass: Utilities.

-- | Enumerates the possible failures from toAst.
data AbstractionErr = GateAbstractionErr Int GateSummaryErr
                    | ArrayLenAbstractionErr Int ExprErr
                    | RValueAbstractionErr Int ExprErr
                    | VoidCallAbstractionErr Int ExprErr
                    | MeasureCallAbstractionErr Int ExprErr
                    | UnknownExprStmt Int
                    | NonPosArrayLen Int
                    | NonLegacyStmt Int
                    | MissingLib Int String
                    | UnexpectedMeasureExpr Int
                    deriving (Show, Eq)

-- | Consumes a line number and OpenQASM header. Returns an error if any
-- quipfuncs.inc call would be invalid relative to the header. Otherwise,
-- nothing is returned.
checkQuipFuncsScope :: QasmHeader -> Int -> Maybe AbstractionErr
checkQuipFuncsScope header ln
    | isLegacy header       = Just $ NonLegacyStmt ln
    | not $ usingQfn header = Just $ MissingLib ln "quipfuncs.inc"
    | otherwise             = Nothing

-- | Consumes a line number, an OpenQASM header, and an OpenQASM gate. If the
-- gate is not accessible with respect to header, then an error is returned.
-- Otherwise, nothing is returned.
checkGateScope :: QasmHeader -> Int -> Gate -> Maybe AbstractionErr
checkGateScope header ln (NamedGate name _ _ _)
    | fromQe1 && noStd && legacy     = Just $ MissingLib ln "qelib1.inc"
    | fromQe1 && noStd && not legacy = Just $ MissingLib ln "stdgates.inc"
    | fromBkp && noBkp && legacy     = Just $ MissingLib ln "bkpgates.inc"
    | fromBkp && noStd && not legacy = Just $ MissingLib ln "stdgates.inc"
    | fromQpr && noQpr               = Just $ MissingLib ln "quipgates.inc"
    | otherwise                      = Nothing
    where legacy  = isLegacy header
          fromQe1 = isQelib1Gate name
          fromBkp = isBackportGate name
          fromQpr = isQuipperGate name
          noStd   = not $ usingStd header
          noBkp   = not $ usingBkp header
          noQpr   = not $ usingQpr header
checkGateScope header ln (GPhaseGate _ _ _)
    | isLegacy header = Just $ NonLegacyStmt ln
    | otherwise       = Nothing 

-- | Consumes a line number, an OpenQASM header, and an rvalue. Returns an
-- error if the rvalue would be invalid relative to the header. Otherwise,
-- nothing is returned.
checkRValueScope :: QasmHeader -> Int -> RValue -> Maybe AbstractionErr
checkRValueScope header ln (QuipMeasure _) = checkQuipFuncsScope header ln
checkRValueScope header ln QuipCInit0      = checkQuipFuncsScope header ln
checkRValueScope header ln QuipCInit1      = checkQuipFuncsScope header ln
checkRValueScope header ln QuipCTerm0      = checkQuipFuncsScope header ln
checkRValueScope header ln QuipCTerm1      = checkQuipFuncsScope header ln
checkRValueScope header ln QuipCDiscard    = checkQuipFuncsScope header ln
checkRValueScope header ln (Measure _)     = Nothing

-- | Consumes a line number, an OpenQASM header, and a void call. Returns an
-- error if the call is invalid relative to the header. Otherwise, nothing is
-- returned.
checkVoidCallScope :: QasmHeader -> Int -> VoidCall -> Maybe AbstractionErr
checkVoidCallScope header ln (QuipQInit0 _)   = checkQuipFuncsScope header ln
checkVoidCallScope header ln (QuipQInit1 _)   = checkQuipFuncsScope header ln
checkVoidCallScope header ln (QuipQTerm0 _)   = checkQuipFuncsScope header ln
checkVoidCallScope header ln (QuipQTerm1 _)   = checkQuipFuncsScope header ln
checkVoidCallScope header ln (QuipQDiscard _) = checkQuipFuncsScope header ln
checkVoidCallScope header ln (VoidReset _)    = Nothing
checkVoidCallScope _      ln (VoidMeasure _)  = Just $ UnexpectedMeasureExpr ln

-------------------------------------------------------------------------------
-- * Primary Pass: Abstraction Pass.

-- | Consumes a line number (ln) and an expression (expr). If expr evaluates to
-- a valid gate (g) that is supported by the translator, then g is returned.
-- Otherwise, an error is returned with line number set to ln.
abstractGate :: QasmHeader -> PerStmtFn GateExpr AbstractionErr
abstractGate header ln expr =
    case exprToGate expr of
        Left (n, gate) -> case validateGate gate of
            Nothing -> case checkGateScope header ln gate of
                Just err -> Right err
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
abstractDecl :: Int -> Type -> String -> Either [AstStmt] AbstractionErr
abstractDecl _  QubitT           decl = Left [AstQubitDecl Nothing decl]
abstractDecl ln (QubitArrT expr) decl =
    case getDeclLen ln expr of
        Left n    -> Left [AstQubitDecl (Just n) decl]
        Right err -> Right err
abstractDecl _  BitT decl = Left [AstBitDecl Nothing decl]
abstractDecl ln (BitArrT expr) decl =
    case getDeclLen ln expr of
        Left n    -> Left [AstBitDecl (Just n) decl]
        Right err -> Right err

-- | Implementation details for abstractAssign.
assignImpl :: QasmHeader -> PerStmtFn (String, Maybe Int, Expr) AbstractionErr
assignImpl header ln (id, idx, expr) = 
    case toRValue expr of
        Left rval -> case checkRValueScope header ln rval of
            Just err -> Right err
            Nothing  -> Left [AstAssign id idx rval]
        Right err -> Right $ RValueAbstractionErr ln err

-- | Consumes a line number (ln), a variable to update, and a value to assign
-- to the variable (rval). If the lvalue and rvalue are valid, then an abstract
-- assignment statement for rval into lval is returned. Otherwise, an error for
-- the first failure is returned. is returned.
abstractAssign :: QasmHeader -> PerStmtFn (LValue, Expr) AbstractionErr
abstractAssign header ln (CVar id, expr) = stmt
    where stmt = assignImpl header ln (id, Nothing, expr)
abstractAssign header ln (CReg id idx, expr) =
    case toArrayIndex idx of
        Left n    -> assignImpl header ln (id, Just n, expr)
        Right err -> Right $ ArrayLenAbstractionErr ln err

-- | Consumes a line number (ln), a variable type, the name of a newly declared
-- variable, and a value to which the variable is initialized (rval). If the
-- declaration and initial value are valid, then an abstract declaration
-- statement for a variable of the name decl, an an abstract initialization
-- statement setting decl to expr, are returned. Otherwise, an error for the
-- first failure is returned. is returned.
abstractInitDecl :: QasmHeader -> PerStmtFn (Type, String, Expr) AbstractionErr
abstractInitDecl header ln (ty, decl, rval) =
    case abstractDecl ln ty decl of
        Left dstmt -> case abstractAssign header ln (CVar decl, rval) of
            Left istmt -> Left $ dstmt ++ istmt
            Right err  -> Right err
        Right err -> Right err

-- | Consumes a line number (ln) and an expression (expr) intended to act as a
-- statement. If expr evaluates to a valid expression statement, then the
-- corresponding abstract statement is returned. Otherwise, an error for the
-- first failure is returned.
abstractExprStmt :: QasmHeader -> PerStmtFn Expr AbstractionErr
abstractExprStmt header ln (Brack expr) = astmts
    where astmts = abstractExprStmt header ln expr
abstractExprStmt header ln (Call name args) =
    case toVoidCall name args of
        Left call -> case checkVoidCallScope header ln call of
            Just err -> Right err
            Nothing  -> Left [AstCall call]
        Right err -> Right $ VoidCallAbstractionErr ln err
abstractExprStmt header ln (QasmMeasure gop) =
    case parseGateOperand gop of
        Left op -> if isLegacy header
                   then Right $ NonLegacyStmt ln
                   else Left [AstCall $ VoidMeasure op]
        Right err -> Right $ MeasureCallAbstractionErr ln err
abstractExprStmt _ ln _ = Right $ UnknownExprStmt ln

-- | Consume a line number (ln) and the argument to a reset statement (expr).
-- If expr evaluates to a valid operand, then a VoidReset parameterized by the
-- operand is returned. Otherwise, an error for the first failure is returned.
abstractResetStmt :: PerStmtFn GateOperand AbstractionErr
abstractResetStmt ln gop =
    case parseGateOperand gop of
        Left op   -> Left [AstCall $ VoidReset op]
        Right err -> Right $ MeasureCallAbstractionErr ln err

-- | Converts a single statement into a sequence of equivalent AST statements.
-- If then conversion fails, then an appropriate abstraction error is returned.
abstractStmt :: QasmHeader -> PerStmtFn Stmt AbstractionErr
abstractStmt header ln (QasmGateStmt expr) = astmts
    where astmts = abstractGate header ln expr
abstractStmt header ln (QasmDeclStmt ty decl)
    | isLegacy header = Right $ NonLegacyStmt ln
    | otherwise       = abstractDecl ln ty decl
abstractStmt _ ln (QasmLDeclStmt ty decl) = astmts
    where astmts = abstractDecl ln ty decl
abstractStmt header ln (QasmAssignStmt lval rval)
    | isLegacy header = Right $ NonLegacyStmt ln
    | otherwise       = abstractAssign header ln (lval, rval)
abstractStmt header ln (QasmLAssignStmt lval rval) = astmts
    where astmts = abstractAssign header ln (lval, rval)
abstractStmt header ln (QasmInitDeclStmt ty decl rval)
    | isLegacy header = Right $ NonLegacyStmt ln
    | otherwise       = abstractInitDecl header ln (ty, decl, rval)
abstractStmt header ln (QasmExprStmt expr) = astmts
    where astmts = abstractExprStmt header ln expr
abstractStmt _ ln (QasmResetStmt expr) = astmts
    where astmts = abstractResetStmt ln expr

-- | Converts a list of statements into an AST. If the conversion fails, then
-- an appropriate abstraction error is returned.
toAst :: QasmHeader -> [Stmt] -> Either [AstStmt] AbstractionErr
toAst header = applyPerLineFn f 1
    where f = abstractStmt header

-------------------------------------------------------------------------------
-- * Secondary Pass: Inverse Elimination

-- Enumeartes the possible failures in elimInv.
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
resolveUnknownInv :: PerStmtFn Gate InversionErr
resolveUnknownInv ln (NamedGate (UserDefined str) _ _ _) = Right err
    where err = UnknownUserDefinedInv ln str
resolveUnknownInv ln (NamedGate name _ _ _) = Right err
    where err = UnknownNativeInv ln name
resolveUnknownInv ln _ = Right err
    where err = UnhandledInvErr ln

-- | Consumes a single AST statement. If the statement is an inverted gate,
-- then the inverse circuit is returned. Otherwise, the statement is returned
-- unchanged. If inlining fails, then an appropriate error is returned.
elimInvImpl :: PerStmtFn AstStmt InversionErr
elimInvImpl ln (AstGateStmt n gate) =
    case invertGate gate of
        Just inv -> Left (inlineInv n inv)
        Nothing  -> resolveUnknownInv ln gate
elimInvImpl _ stmt = Left [stmt]

-- | Inlines all inverse gates in a list of AST statements. If inlining fails,
-- then an appropriate error is returned.
elimInv :: [AstStmt] -> Either [AstStmt] InversionErr
elimInv = applyPerLineFn elimInvImpl 1

-------------------------------------------------------------------------------
-- * Secondary Pass: Integral Power Elimination

-- | Inlines the power modifers of a single AST statements.
elimPowImpl :: AstStmt -> [AstStmt]
elimPowImpl (AstGateStmt 0 gate) = [AstGateStmt 0 gate]
elimPowImpl (AstGateStmt n gate) = replicate n $ AstGateStmt 0 gate
elimPowImpl stmt                 = [stmt]

-- | Inlines all power modifers in a list of AST statements.
elimPow :: [AstStmt] -> [AstStmt]
elimPow = applySafePerLineFn elimPowImpl

-------------------------------------------------------------------------------
-- * Secondary Pass: Call Elimination

-- | Enumerates the possible failures in elimFun.
data InlineErr = FailedToEval Int String deriving (Show, Eq)

-- | Takes as input a line number and a gate. Implements elimFunImpl for all
-- parameters in (AstGateStmt _ gate).
elimInGate :: Int -> Gate -> Either Gate InlineErr
elimInGate ln (NamedGate name args ops mods) =
    case elimCallsInArglist args of
        Left args' -> Left $ NamedGate name args' ops mods
        Right name -> Right $ FailedToEval ln name
elimInGate ln (GPhaseGate arg ops mods) =
    case elimCallsInExpr arg of
        Left arg'  -> Left $ GPhaseGate arg' ops mods
        Right name -> Right $ FailedToEval ln name

-- | Implements elimFunImpl for the statement (AstCall call).
elimVoidCall :: VoidCall -> Either [AstStmt] InlineErr
elimVoidCall (QuipQInit1 op) = Left [setTo0, setTo1]
    where setTo0 = AstCall $ VoidReset op
          setTo1 = AstGateStmt 0 $ NamedGate GateX [] [op] nullGateMod
elimVoidCall (QuipQInit0 op)      = Left [AstCall $ VoidReset op]
elimVoidCall (QuipQTerm0 _)       = Left []
elimVoidCall (QuipQTerm1 _)       = Left []
elimVoidCall (QuipQDiscard _)     = Left []
elimVoidCall call@(VoidReset _)   = Left [AstCall call]
elimVoidCall call@(VoidMeasure _) = Left [AstCall call]

-- | Takes as input a line number, the name and index of an lvalue, and an
-- rvalue. Implements elimFunImpl for the statement (AstAssign id idx rval).
elimInAssign :: String -> Maybe Int -> RValue -> Either [AstStmt] InlineErr
elimInAssign id idx (QuipMeasure op) = Left [AstAssign id idx $ Measure op]
elimInAssign id idx QuipCInit0       = error "Classical computation."
elimInAssign id idx QuipCInit1       = error "Classical computation."
elimInAssign id idx QuipCTerm0       = Left []
elimInAssign id idx QuipCTerm1       = Left []
elimInAssign id idx QuipCDiscard     = Left []
elimInAssign id idx rval@(Measure _) = Left [AstAssign id idx rval]

-- | Inlines function calls that are not built into version 2.0 of OpenQASM,
-- for a single AST statement.
elimFunImpl :: PerStmtFn AstStmt InlineErr
elimFunImpl ln (AstGateStmt n gate) = 
    case elimInGate ln gate of
        Left gate' -> Left [AstGateStmt n gate']
        Right err  -> Right err
elimFunImpl _ (AstAssign id idx rval) = elimInAssign id idx rval
elimFunImpl _ (AstCall call)          = elimVoidCall call
elimFunImpl _ stmt                    = Left [stmt] 

-- | Inlines function calls that are not built into version 2.0 of OpenQASM.
elimFun :: [AstStmt] -> Either [AstStmt] InlineErr
elimFun = applyPerLineFn elimFunImpl 1

-------------------------------------------------------------------------------
-- * Secondary Pass: Phase Elimination

-- | Enumerates the possible failures in toLsc.
data ToLscErr = UnexpectedPowerMod Int
              | LscRewriteFailure Int LscGateErr
              deriving (Show, Eq)

-- | Rewrites a single AST gate statement to conform with the lattice surgery
-- compiler, or returns an error when this is not possible.
toLscImpl :: PerStmtFn AstStmt ToLscErr
toLscImpl ln (AstGateStmt 0 gate) =
    case lscRewriteGate gate of
        Left gates -> Left $ map (AstGateStmt 0) gates
        Right err  -> Right $ LscRewriteFailure ln err
toLscImpl ln (AstGateStmt _ _) = Right $ UnexpectedPowerMod ln
toLscImpl _  stmt              = Left [stmt]

-- | Rewrites the gates of an OpenQASM program so that it is amenable to the
-- lattice surgery compiler. Requires that the program has already undergone
-- a round translation, with maximum inlining.
toLsc :: [AstStmt] -> Either [AstStmt] ToLscErr
toLsc = applyPerLineFn toLscImpl 1
