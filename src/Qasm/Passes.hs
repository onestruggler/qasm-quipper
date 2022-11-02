-- | Static analysis passes for OpenQASM 3 compilation.

module Qasm.Passes where

import Qasm.AST (AstStmt(..))
import Qasm.Expression (ExprErr, toConstInt)
import Qasm.Gate (GateSummaryErr, exprToGate, validateGate)
import Qasm.Language (Expr, GateExpr, Stmt(..), Type(..))

-------------------------------------------------------------------------------
-- * Pass 1: Abstraction Pass.

data AbstractionErr = GateAbstractionErr Int GateSummaryErr
                    | ArrayLenAbstractionErr Int ExprErr
                    | NonPosArrayLen Int
                    deriving (Show, Eq)

-- | Consumes a line number (ln) and an expression (expr). If expr evaluates to
-- a valid gate (g) that is supported by the translator, then g is returned.
-- Otherwise, an error is returned with line number set to ln.
abstractQasmGate :: Int -> GateExpr -> Either AstStmt AbstractionErr
abstractQasmGate ln expr =
    case exprToGate expr of
        Left (n, gate) -> case validateGate gate of
            Nothing  -> Left (AstGateStmt n gate)
            Just err -> Right (GateAbstractionErr ln err)
        Right err -> Right (GateAbstractionErr ln err)

-- | Consumes a line number (ln), a variable type, and the name of the declared
-- variable (decl). If the declaration if valid, then a abstract declaration
-- for a variable with name decl is returned. The constructor of the abstract
-- declaration is determined by the type. If the type is invalid, then an error
-- is returned with line number set to ln.
abstractQasmDecl :: Int -> Type -> String -> Either AstStmt AbstractionErr
abstractQasmDecl _  QubitT           decl = Left (AstQubitDecl Nothing decl)
abstractQasmDecl ln (QubitArrT expr) decl =
    case toConstInt expr of
        Left n -> if n > 0
                  then Left (AstQubitDecl (Just n) decl)
                  else Right (NonPosArrayLen ln)
        Right err -> Right (ArrayLenAbstractionErr ln err)

-- | Converts a single statement into a sequence of equivalent AST statements.
-- If then conversion fails, then an appropriate abstraction error is returned.
abstractStmt :: Int -> Stmt -> Either AstStmt AbstractionErr
abstractStmt ln (QasmGateStmt expr)    = abstractQasmGate ln expr
abstractStmt ln (QasmDeclStmt ty decl) = abstractQasmDecl ln ty decl

-- Converts a list of statements into an AST. If the conversion fails, then an
-- appropriate abstraction error is returned.
toAst :: Int -> [Stmt] -> Either [AstStmt] AbstractionErr
toAst ln []           = Left []
toAst ln (stmt:stmts) =
    case abstractStmt ln stmt of
        Left astStmt -> case toAst (ln + 1) stmts of
            Left ast  -> Left (astStmt : ast)
            Right err -> Right err
        Right err -> Right err
