module QasmLang where

data Expr = DecInt String deriving (Show)

data GateOperand = QVar String
                 | QReg String Expr
                 deriving (Show)

data Gate = NamedGate String [GateOperand]
          | CtrlMod (Maybe Expr) Gate
          | NegCtrlMod (Maybe Expr) Gate
          | InvMod Gate
          | PowMod Expr Gate
          deriving (Show)
