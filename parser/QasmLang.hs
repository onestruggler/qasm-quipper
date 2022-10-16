module QasmLang where

data Expr = Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Div Expr Expr
          | Brack Expr
          | Negate Expr
          | Pi
          | DecInt String
          | QasmId String
          deriving (Show)

data GateOperand = QVar String
                 | QReg String Expr
                 deriving (Show)

data Gate = NamedGateOp String [Expr] [GateOperand]
          | GPhaseOp Expr [GateOperand]
          | CtrlMod (Maybe Expr) Gate
          | NegCtrlMod (Maybe Expr) Gate
          | InvMod Gate
          | PowMod Expr Gate
          deriving (Show)
