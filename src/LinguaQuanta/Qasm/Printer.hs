-- | Printing functions for the OpenQASM 3 AST.

module LinguaQuanta.Qasm.Printer (printAst) where

-------------------------------------------------------------------------------
-- * Import Section.

import Data.List (intercalate)
import LinguaQuanta.Qasm.AST (AstStmt(..))
import LinguaQuanta.Qasm.Gate
  ( Gate(..)
  , GateMod(..)
  , Sign(..)
  )
import LinguaQuanta.Qasm.GateName (GateName(..))
import LinguaQuanta.Qasm.Language
  ( Expr(..)
  , GateOperand(..)
  )

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

-- | Consumes a gate name and a legacy flag. Returns the string representation
-- of the name. If the legacy flag is set, then an OpenQASM 2.0 compatible name
-- is returned (often these names are the same).
printName :: Bool -> GateName -> String
printName _     (UserDefined str) = str
printName _     GateX             = "x"
printName True  GateCX            = "CX"
printName False GateCX            = "cx"
printName _     GateCCX           = "ccx"
printName _     GateY             = "y"
printName _     GateCY            = "cy"
printName _     GateZ             = "z"
printName _     GateCZ            = "cz"
printName _     GateH             = "h"
printName _     GateCH            = "ch"
printName _     GateSwap          = "swap"
printName _     GateCSwap         = "cswap"
printName _     GateS             = "s"
printName _     GateSdg           = "sdg"
printName _     GateSX            = "sx"
printName _     GateT             = "t"
printName _     GateTdg           = "tdg"
printName _     GateID            = "id"
printName _     GateQuipIX        = "quip_ix"
printName _     GateQuipOmega     = "quip_omega"
printName _     GateQuipE         = "quip_e"
printName _     GateQuipW         = "quip_w"
printName _     GateRX            = "rx"
printName _     GateCRX           = "crx"
printName _     GateRY            = "ry"
printName _     GateCRY           = "cry"
printName _     GateRZ            = "rz"
printName _     GateCRZ           = "crz"
printName _     GateP             = "p"
printName _     GateCP            = "cp"
printName _     GatePhase         = "phase"
printName _     GateCPhase        = "cphase"
printName _     GateU             = "u"
printName _     GateCU            = "cu"
printName _     GateU1            = "u1"
printName _     GateU2            = "u2"
printName _     GateU3            = "u3"
printName _     GateQuipRZ        = "quip_rz"

-- | Converts a gate to its syntactic representation. Modifiers are expanded.
printGate :: Bool -> Gate -> String
printGate legacy (NamedGate name params operands mods) = mstr ++ body
    where mstr = printMods mods 
          nstr = printName legacy name
          pstr = printParams params
          ostr = printOperands operands
          body = nstr ++ pstr ++ " " ++ ostr
printGate _ (GPhaseGate param operands mods) = mstr ++ body
    where mstr = printMods mods 
          pstr = printParams [param]
          ostr = printOperands operands
          body = "gphase" ++ pstr ++ " " ++ ostr

-------------------------------------------------------------------------------
-- * Statement Printing.

-- | Converts a qubit array of a given length to its syntactic representation.
printQubitType :: Maybe Int -> String
printQubitType Nothing  = "qubit"
printQubitType (Just n) = printQubitType Nothing ++ "[" ++ show n ++ "]"

-- | Consumes the length associated with a variable declaration. If the length
-- is just an integer, the the corresponding designator string is returned.
-- Otherwise, nothing is returned.
printArrLen :: Maybe Int -> String
printArrLen Nothing  = ""
printArrLen (Just n) = "[" ++ show n ++ "]"

-- | Consumes a legacy flag, array length (len), and qubit declaration name
-- (decl). If the legacy flag is false, then a qubit declaration is returned
-- with the designator corresponding to len. Otherwise, a qreg declaration is
-- returned using the same len and decl.
printQubitDecl :: Bool -> Maybe Int -> String -> String
printQubitDecl False len decl = "qubit" ++ printArrLen len ++ " " ++ decl
printQubitDecl True  len decl = "qreg " ++ decl ++ printArrLen len

-- | Concretizes a statement, and produces its syntactic representation.
printAstStmt :: Bool -> AstStmt -> String
printAstStmt legacy (AstGateStmt n gate) = powMod ++ gateStr ++ ";"
    where powMod = if n == 0 then "" else "pow(" ++ show n ++ ") @ "
          gateStr = printGate legacy gate
printAstStmt legacy (AstQubitDecl len decl) = printQubitDecl legacy len decl

-- | Concretizes each statement, and produces its syntactic representation.
printAst :: Bool -> [AstStmt] -> [String]
printAst legacy = map (printAstStmt legacy)
