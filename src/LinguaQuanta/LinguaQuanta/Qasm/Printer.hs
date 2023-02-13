-- | Printing functions for the OpenQASM 3 AST.

module LinguaQuanta.Qasm.Printer (printAst) where

-------------------------------------------------------------------------------
-- * Import Section.

import Data.List (intercalate)
import LinguaQuanta.Qasm.AST (AstStmt(..))
import LinguaQuanta.Qasm.Gate
  ( Gate(..)
  , GateMod(..)
  , Operand(..)
  , Sign(..)
  )
import LinguaQuanta.Qasm.GateName (GateName(..))
import LinguaQuanta.Qasm.Language (Expr(..))
import LinguaQuanta.Qasm.Operand
  ( Operand(..)
  , RValue(..)
  , VoidCall(..)
  )

-------------------------------------------------------------------------------
-- * Expression Printing.

-- | Converts an expression to its syntactic representation.
printExpr :: Expr -> String
printExpr (Plus lhs rhs)      = printExpr lhs ++ " + " ++ printExpr rhs
printExpr (Minus lhs rhs)     = printExpr lhs ++ " - " ++ printExpr rhs
printExpr (Times lhs rhs)     = printExpr lhs ++ " * " ++ printExpr rhs
printExpr (Div lhs rhs)       = printExpr lhs ++ " / " ++ printExpr rhs
printExpr (Brack expr)        = "(" ++ printExpr expr ++ ")"
printExpr (Negate expr)       = "-" ++ printExpr expr
printExpr Euler               = "\x2107"
printExpr Pi                  = "\x3C0"
printExpr Tau                 = "\x3C4"
printExpr (DecFloat str)      = str
printExpr (DecInt str)        = str
printExpr (QasmId id)         = id
printExpr (QasmCell id index) = id ++ "[" ++ printExpr index ++ "]"
printExpr (Call id args)      = id ++ printParams args

-- | Converts a parameter list to a list of syntactic expressions, contained
-- within a pair of parentheses. If the list is empty, then an empty string is
-- returns without parentheses. 
printParams :: [Expr] -> String
printParams []     = ""
printParams params = "(" ++ list ++ ")"
    where list = intercalate ", " $ map printExpr params

-- | Converts a gate operand to its syntactic representation.
printOperand :: Operand -> String
printOperand (QRef str)       = str
printOperand (Cell str index) = str ++ "[" ++ show index ++ "]"

-- | Converts a list of operands to its syntactic representation.
printOperands :: [Operand] -> String
printOperands []       = ""
printOperands operands = intercalate ", " $ map printOperand operands

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
-- * LValue and RValue Printing.

-- | Consumes a legacy flag, the name of a declaration, and optionally an index
-- into the declaration (e.g., if the declaration is an array). Returns a
-- textual representation of the corresponding lvalue, adhering to the legacy
-- flag.
printLValue :: Bool -> String -> Maybe Int -> String
printLValue _ id Nothing      = printOperand $ QRef id
printLValue _ id (Just index) = printOperand $ Cell id index

-- | Consumes a legacy flag and an rvalue. Returns a textual representation of
-- the rvalue, adhering to the legacy flag.
printRValue :: Bool -> RValue -> String
printRValue _ (QuipMeasure operand) = "QMeas(" ++ printOperand operand ++ ")"
printRValue _ QuipCInit0            = "CInit0()"
printRValue _ QuipCInit1            = "CInit1()"
printRValue _ QuipCTerm0            = "CTerm0()"
printRValue _ QuipCTerm1            = "CTerm1()"
printRValue _ QuipCDiscard          = "CDiscard()"

-------------------------------------------------------------------------------
-- * Statement Printing.

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
printQubitDecl False len decl = "qubit" ++ printArrLen len ++ " " ++ decl ++ ";"
printQubitDecl True  len decl = "qreg " ++ decl ++ printArrLen len ++ ";"

-- | Consumes a legacy flag, array length (len), and classical bit declaration
-- name (decl). If the legacy flag is flase, then a classical bit declaration
-- is returned with the designator corresponding to len. Otherwise, a creg
-- declaration is returned using the same len and decl.
printBitDecl :: Bool -> Maybe Int -> String -> String
printBitDecl False len decl = "bit"  ++ printArrLen len ++ " " ++ decl ++ ";"
printBitDecl True  len decl = "creg" ++ decl ++ printArrLen len ++ ";"

-- | Consumes a legacy flag, the name of a declaration, optionally an index
-- into the declaration (e.g., if the declaration is an array), and an rvalue.
-- Returns a textual representation of assigning the rvalue to the given index
-- of the declaration, adhering to the legacy flag.
printAssign :: Bool -> String -> Maybe Int -> RValue -> String
printAssign legacy id index rval = lstr ++ " = " ++ rstr ++ ";"
    where lstr = printLValue legacy id index
          rstr = printRValue legacy rval

-- | Consumes a legacy flag and an instance of a void call. Returns a textual
-- representation of the void call, adhering to the legacy flag.
printCall :: Bool -> VoidCall -> String
printCall _ (QuipQInit0 op)   = "QInit0(" ++ printOperand op ++ ");"
printCall _ (QuipQInit1 op)   = "QInit1(" ++ printOperand op ++ ");"
printCall _ (QuipQTerm0 op)   = "QTerm0(" ++ printOperand op ++ ");"
printCall _ (QuipQTerm1 op)   = "QTerm1(" ++ printOperand op ++ ");"
printCall _ (QuipQDiscard op) = "QDiscard(" ++ printOperand op ++ ");"

-- | Concretizes a statement, and produces its syntactic representation.
printAstStmt :: Bool -> AstStmt -> String
printAstStmt legacy (AstGateStmt n gate) = powMod ++ gateStr ++ ";"
    where powMod = if n == 0 then "" else "pow(" ++ show n ++ ") @ "
          gateStr = printGate legacy gate
printAstStmt legacy (AstQubitDecl len decl)   = printQubitDecl legacy len decl
printAstStmt legacy (AstBitDecl len decl)     = printBitDecl legacy len decl
printAstStmt legacy (AstAssign id index rval) = printAssign legacy id index rval
printAstStmt legacy (AstCall call)            = printCall legacy call

-- | Concretizes each statement, and produces its syntactic representation.
printAst :: Bool -> [AstStmt] -> [String]
printAst legacy = map $ printAstStmt legacy
