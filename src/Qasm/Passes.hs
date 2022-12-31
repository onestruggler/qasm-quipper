-- | Static analysis passes for OpenQASM 3 compilation.

module Qasm.Passes
  ( AbstractionErr(..)
  , InversionErr(..)
  , elimInv
  , elimPow
  , toAst
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import Qasm.AST (AstStmt(..))
import Qasm.Expression
  ( ExprErr
  , toConstInt
  )
import Qasm.Inversion (invertGate)
import Qasm.Gate
  ( Gate(NamedGate)
  , GateSummaryErr
  , exprToGate
  , validateGate
  )
import Qasm.GateName (GateName(UserDefined))
import Qasm.Language
  ( Expr
  , GateExpr
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
                    | NonPosArrayLen Int
                    deriving (Show, Eq)

-- | Consumes a line number (ln) and an expression (expr). If expr evaluates to
-- a valid gate (g) that is supported by the translator, then g is returned.
-- Otherwise, an error is returned with line number set to ln.
abstractQasmGate :: Int -> GateExpr -> Either [AstStmt] AbstractionErr
abstractQasmGate ln expr =
    case exprToGate expr of
        Left (n, gate) -> case validateGate gate of
            Nothing  -> Left [AstGateStmt n gate]
            Just err -> Right (GateAbstractionErr ln err)
        Right err -> Right (GateAbstractionErr ln err)

-- | Consumes a line number (ln), a variable type, and the name of the declared
-- variable (decl). If the declaration if valid, then a abstract declaration
-- for a variable with name decl is returned. The constructor of the abstract
-- declaration is determined by the type. If the type is invalid, then an error
-- is returned with line number set to ln.
abstractQasmDecl :: Int -> Type -> String -> Either [AstStmt] AbstractionErr
abstractQasmDecl _  QubitT           decl = Left [AstQubitDecl Nothing decl]
abstractQasmDecl ln (QubitArrT expr) decl =
    case toConstInt expr of
        Left n -> if n > 0
                  then Left [AstQubitDecl (Just n) decl]
                  else Right (NonPosArrayLen ln)
        Right err -> Right (ArrayLenAbstractionErr ln err)

-- | Converts a single statement into a sequence of equivalent AST statements.
-- If then conversion fails, then an appropriate abstraction error is returned.
abstractStmt :: Int -> Stmt -> Either [AstStmt] AbstractionErr
abstractStmt ln (QasmGateStmt expr)    = abstractQasmGate ln expr
abstractStmt ln (QasmDeclStmt ty decl) = abstractQasmDecl ln ty decl

-- | Converts a list of statements into an AST. If the conversion fails, then
-- an appropriate abstraction error is returned.
toAst :: [Stmt] -> Either [AstStmt] AbstractionErr
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
elimInvImpl _  stmt = Left [stmt]

-- | Inlines all inverse gates in a list of AST statements. If inlining fails,
-- then an appropriate error is returned.
elimInv :: [AstStmt] -> Either [AstStmt] InversionErr
elimInv = applyPerLinePass elimInvImpl 1

-------------------------------------------------------------------------------
-- * Secondary Pass: Integral Power Elimination

-- | Inlines the power modifers of a single AST statements.
elimPowImpl :: AstStmt -> [AstStmt]
elimPowImpl (AstGateStmt 0 gate) = [AstGateStmt 0 gate]
elimPowImpl (AstGateStmt n gate) = (replicate n $ AstGateStmt 0 gate)
elimPowImpl stmt                 = [stmt]

-- | Inlines all power modifers in a list of AST statements.
elimPow :: [AstStmt] -> [AstStmt]
elimPow = applySafePerLinePass elimPowImpl
