-- | Static analysis passes for OpenQASM 3 compilation.

module Qasm.Passes where

import Qasm.AST (AstStmt(..))
import Qasm.Expression (ExprErr, toConstInt)
import Qasm.Gate (GateSummaryErr, exprToGate, validateGate)
import Qasm.Language (Expr, GateExpr, Stmt(..), Type(..))

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

-------------------------------------------------------------------------------
-- * Pass 1: Abstraction Pass.

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

-- Converts a list of statements into an AST. If the conversion fails, then an
-- appropriate abstraction error is returned.
toAst :: [Stmt] -> Either [AstStmt] AbstractionErr
toAst = applyPerLinePass abstractStmt 0
