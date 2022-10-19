-- | Printing Methods for the OpenQASM 3 AST.

module Qasm.Printer (printAst) where

import Data.List (intercalate)
import Qasm.AST (AstStmt(..))
import Qasm.Gate (Gate(..), GateMod(..), Sign(..))
import Qasm.GateName (GateName(..))
import Qasm.Language(Expr(..), GateOperand(..), Type(..))

-------------------------------------------------------------------------------
-- * Expression Printing.

-- | Converts an expression to its syntactic representation.
printExpr :: Expr -> String
printExpr (Plus lhs rhs)  = printExpr lhs ++ " + " ++ printExpr rhs
printExpr (Minus lhs rhs) = printExpr lhs ++ " - " ++ printExpr rhs
printExpr (Times lhs rhs) = printExpr lhs ++ " * " ++ printExpr rhs
printExpr (Div lhs rhs)   = printExpr lhs ++ " / " ++ printExpr rhs
printExpr (Brack expr)    = "(" ++ printExpr expr ++ ")"
printExpr (Negate expr)   = "-" ++ printExpr expr
printExpr Pi              = "\x3C0"
printExpr (DecInt str)    = str
printExpr (QasmId id)     = id

-- | Converts a parameter list to a list of syntactic expressions, contained
-- within a pair of parentheses. If the list is empty, then an empty string is
-- returns without parentheses. 
printParams :: [Expr] -> String
printParams []     = ""
printParams params = "(" ++ list ++ ")"
    where list = intercalate ", " (map printExpr params)

-- | Converts a gate operand to its syntactic representation.
printOperand :: GateOperand -> String
printOperand (QVar str)       = str
printOperand (QReg str index) = str ++ "[" ++ printExpr index ++ "]"

-- | Converts a list of operands to its syntactic representation.
printOperands :: [GateOperand] -> String
printOperands []       = ""
printOperands operands = intercalate ", " (map printOperand operands)

-------------------------------------------------------------------------------
-- * Gate Printing.

-- | Converts a modifier into a sequence of modifer expressions.
printMods :: GateMod -> String
printMods (GateMod False [])      = ""
printMods (GateMod False (Pos:c)) = "ctrl @ " ++ printMods (GateMod False c)
printMods (GateMod False (Neg:c)) = "negctrl @ " ++ printMods (GateMod False c)
printMods (GateMod True c)        = "inv @ " ++ printMods (GateMod False c)

-- | Converts a gate name to its string representation.
printName :: GateName -> String
printName (UserDefined str) = str
printName GateX             = "x"
printName GateCX            = "cx"
printName GateCCX           = "ccx"
printName GateY             = "y"
printName GateCY            = "cy"
printName GateZ             = "z"
printName GateCZ            = "cz"
printName GateH             = "h"
printName GateCH            = "ch"
printName GateSwap          = "swap"
printName GateCSwap         = "cswap"
printName GateS             = "s"
printName GateSdg           = "sdg"
printName GateSX            = "sx"
printName GateT             = "t"
printName GateTdg           = "tdg"
printName GateID            = "id"
printName GateQuipIX        = "quip_ix"
printName GateQuipOmega     = "quip_omega"
printName GateQuipE         = "quip_e"
printName GateQuipW         = "quip_w"
printName GateRX            = "rx"
printName GateCRX           = "crx"
printName GateRY            = "ry"
printName GateCRY           = "cry"
printName GateRZ            = "rz"
printName GateCRZ           = "crz"
printName GateP             = "p"
printName GateCP            = "cp"
printName GatePhase         = "phase"
printName GateCPhase        = "cphase"
printName GateU             = "u"
printName GateCU            = "cu"
printName GateU1            = "u1"
printName GateU2            = "u2"
printName GateU3            = "u3"
printName GateQuipRZ        = "quip_rz"

-- | Converts a gate to its syntactic representation. Modifiers are expanded.
printGate :: Gate -> String
printGate (NamedGate name params operands mods) = mstr ++ body
    where mstr = printMods mods 
          nstr = printName name
          pstr = printParams params
          ostr = printOperands operands
          body = nstr ++ pstr ++ " " ++ ostr
printGate (GPhaseGate param operands mods) = mstr ++ body
    where mstr = printMods mods 
          pstr = printParams [param]
          ostr = printOperands operands
          body = "gphase" ++ pstr ++ " " ++ ostr

-------------------------------------------------------------------------------
-- * Statement Printing.

-- | Converts a type to its syntactic representation.
printType :: Type -> String
printType QubitT         = "qubit"
printType (QubitArrT sz) = "qubit" ++ "[" ++ printExpr sz ++ "]"

-- | Concretizes a statement, and produces its syntactic representation.
printAstStmt :: AstStmt -> String
printAstStmt (AstGateStmt n gate) = pownMod ++ gateStr ++ ";"
    where pownMod = if n == 0 then "" else "pow(" ++ (show n) ++ ") @ "
          gateStr = printGate gate
printAstStmt (AstDeclStmt ty decl) = (printType ty) ++ " " ++ decl ++ ";"

-- | Concretizes each statement, and produces its syntactic representation.
printAst :: [AstStmt] -> [String]
printAst = map printAstStmt
