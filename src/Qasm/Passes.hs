-- | Static analysis passes for OpenQASM 3 compilation.

module Qasm.Passes where

import Qasm.AST (AstStmt(..))
import Qasm.Gate (GateSummaryErr(..), exprToGate, validateGate)
import Qasm.Language (Stmt(..))

-------------------------------------------------------------------------------
-- * Pass 1: Abstraction Pass.

data AbstractionErr = GateAbstractionErr Int GateSummaryErr deriving (Show, Eq)

-- | Converts a single statement into a sequence of equivalent AST statements.
-- If then conversion fails, then an appropriate abstraction error is returned.
abstractStmt :: Int -> Stmt -> Either AstStmt AbstractionErr
abstractStmt ln (QasmGateStmt expr) =
    case exprToGate expr of
        Left (n, gate) -> case validateGate gate of
            Nothing  -> Left (AstGateStmt n gate)
            Just err -> Right (GateAbstractionErr ln err)
        Right err -> Right (GateAbstractionErr ln err)
abstractStmt _ (QasmDeclStmt ty decl) = Left (AstDeclStmt ty decl)

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
