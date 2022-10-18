-- | Haskell representation of the OpenQASM 3 grammar.

module Qasm.Language where

data Expr = Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Div Expr Expr
          | Brack Expr
          | Negate Expr
          | Pi
          | DecInt String
          | QasmId String
          deriving (Show, Eq)

data GateOperand = QVar String
                 | QReg String Expr
                 deriving (Show, Eq)

data GateExpr = NamedGateOp String [Expr] [GateOperand]
              | GPhaseOp Expr [GateOperand]
              | CtrlMod (Maybe Expr) GateExpr
              | NegCtrlMod (Maybe Expr) GateExpr
              | InvMod GateExpr
              | PowMod Expr GateExpr
              deriving (Show, Eq)

data Type = QubitT
          | QubitArrT Expr
          deriving (Show, Eq)

data Stmt = QasmGateStmt GateExpr
          | QasmDeclStmt Type String
          deriving (Show, Eq)
