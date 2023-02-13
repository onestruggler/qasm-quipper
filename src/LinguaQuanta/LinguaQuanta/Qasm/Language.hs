-- | Haskell representation of the OpenQASM 3 grammar.

module LinguaQuanta.Qasm.Language where

-------------------------------------------------------------------------------
-- * Expressions.

data Expr = Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Div Expr Expr
          | Brack Expr
          | Negate Expr
          | Euler
          | Pi
          | Tau
          | DecFloat String
          | DecInt String
          | QasmId String
          | QasmCell String Expr
          | Call String [Expr]
          deriving (Show, Eq)

data GateOperand = QVar String
                 | QReg String Expr
                 deriving (Show, Eq)

data LValue = CVar String
            | CReg String Expr
            deriving (Show, Eq)

data GateExpr = NamedGateOp String [Expr] [GateOperand]
              | GPhaseOp Expr [GateOperand]
              | CtrlMod (Maybe Expr) GateExpr
              | NegCtrlMod (Maybe Expr) GateExpr
              | InvMod GateExpr
              | PowMod Expr GateExpr
              deriving (Show, Eq)

data Type = BitT
          | BitArrT Expr
          | QubitT
          | QubitArrT Expr
          deriving (Show, Eq)

-------------------------------------------------------------------------------
-- * Statements.

data Stmt = QasmGateStmt GateExpr
          | QasmDeclStmt Type String
          | QasmAssignStmt LValue Expr
          | QasmInitDeclStmt Type String Expr
          | QasmExprStmt Expr
          deriving (Show, Eq)
