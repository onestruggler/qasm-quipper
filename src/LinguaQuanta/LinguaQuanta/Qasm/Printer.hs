-- | Printing functions for the OpenQASM 3 AST.

module LinguaQuanta.Qasm.Printer (printAst) where

-------------------------------------------------------------------------------
-- * Import Section.

import Data.List (intercalate)
import LinguaQuanta.Qasm.AST (AstStmt(..))
import LinguaQuanta.Qasm.Expression
  ( euler
  , toArrayIndex
  )
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
-- * Tools for Legacy Support.

_FUNS_TOOL  = "elim_funs"
_POWS_TOOL  = "elim_pows"
_INVS_TOOL  = "elim_invs"
_CTRLS_TOOL = "elim_ctrls"

-- | Produces an error and directs the user to a tool to resolve the error.
toolError :: String -> String -> String
toolError tool details = error $ details ++ " " ++ line
    where line = "Run " ++ tool ++ " before generating legacy output."

-------------------------------------------------------------------------------
-- * Function Printing.

-- | Produces an error when inlining fails.
inlineError :: String -> String
inlineError name = toolError _FUNS_TOOL line
    where line = "Function " ++ name ++ " not supported in legacy output."

-- | Prints quantum ancilla calls and measurements.
printQHelper :: Bool -> String -> Operand -> String
printQHelper True  name _  = inlineError name
printQHelper False name op = name ++ "(" ++ printOperand op ++ ")"

-- | Prints classical ancilla calls.
printCHelper :: Bool -> String -> String
printCHelper True  name = inlineError name
printCHelper False name = name ++ "()"

-------------------------------------------------------------------------------
-- * Expression Printing.

-- | Formates a binary operation as: lhs op rhs.
printBinaryOp :: Bool -> String -> Expr -> Expr -> String
printBinaryOp legacy op lhs rhs = lstr ++ " " ++ op ++ " " ++ rstr
    where lstr = printExpr legacy lhs
          rstr = printExpr legacy rhs

-- | Returns true if a call name is built into OpenQASM 2.0.
isLegacyCall :: String -> Bool
isLegacyCall "sin"  = True
isLegacyCall "cos"  = True
isLegacyCall "tan"  = True
isLegacyCall "exp"  = True
isLegacyCall "ln"   = True
isLegacyCall "sqrt" = True
isLegacyCall _      = False

-- | Takes as input a legacy flag, the name of a function, and a list of
-- arguments to the expression. If the legacy flag is false, or the call is
-- build into OpenQASM 2.0, then a syntactic representation of the call is
-- returned, where the legacy flag is used to convert each argument. Otherwise,
-- an error is raised.
printExprCall :: Bool -> String -> [Expr] -> String
printExprCall legacy id args = if not legacy || isLegacyCall id
                               then id ++ printParams legacy "()" args
                               else inlineError id

-- | Takes as input a legacy flag, the contents of a QasmCell, namely the
-- declaration name and the index expression. If the legacy flag is not set
-- then the QasmCell is printed verbatim. Otherwise, the index expression is
-- converted to a constant integer. If index conversion fails, then an error
-- is raised.
printQasmCell :: Bool -> String -> Expr -> String
printQasmCell True id idx =
    case toArrayIndex idx of
        Left n    -> id ++ "[" ++ show idx ++ "]"
        Right err -> error $ line ++ show err
    where line = "Non-constant array indices supported in legacy mode: "
printQasmCell False id idx = id ++ "[" ++ printExpr False idx ++ "]"

-- | Takes as input a legacy flag an an expression. Converts the expression
-- to its syntactic representation, according to the legacy flag. If the
-- expression has no legacy equivalent, then an error is raised.
printExpr :: Bool -> Expr -> String
printExpr legacy (Plus lhs rhs)    = printBinaryOp legacy "+" lhs rhs
printExpr legacy (Minus lhs rhs)   = printBinaryOp legacy "-" lhs rhs
printExpr legacy (Times lhs rhs)   = printBinaryOp legacy "*" lhs rhs
printExpr legacy (Div lhs rhs)     = printBinaryOp legacy "/" lhs rhs
printExpr legacy (Brack expr)      = "(" ++ printExpr legacy expr ++ ")"
printExpr legacy (Negate expr)     = "-" ++ printExpr legacy expr
printExpr True   Euler             = printExpr True euler
printExpr False  Euler             = "\x2107"
printExpr True   Pi                = "pi"
printExpr False  Pi                = "\x3C0"
printExpr True   Tau               = "(2 * pi)"
printExpr False  Tau               = "\x3C4"
printExpr True   (DecFloat str)    = filter (/= '_') str
printExpr False  (DecFloat str)    = str
printExpr True   (DecInt str)      = filter (/= '_') str
printExpr False  (DecInt str)      = str
printExpr _      (QasmId id)       = id
printExpr legacy (QasmCell id idx) = printQasmCell legacy id idx
printExpr legacy (Call id args)    = printExprCall legacy id args

-- | Takes as input a legacy flag, a parameter list, and a base case string.
-- The parameter list is converted to a list of syntactic expressions,
-- contained within a pair of parentheses. If the list is empty, then the base
-- case string is returns. The legacy flag is used to convert each expression
-- to its syntactic representation.
printParams :: Bool -> String -> [Expr] -> String
printParams _      bc []     = bc
printParams legacy bc params = "(" ++ list ++ ")"
    where mapf = printExpr legacy
          list = intercalate ", " $ map mapf params

-- | Converts a gate operand to its syntactic representation.
printOperand ::  Operand -> String
printOperand (QRef str)     = str
printOperand (Cell str idx) = str ++ "[" ++ show idx ++ "]"

-- | Converts a list of operands to its syntactic representation.
printOperands :: [Operand] -> String
printOperands []       = ""
printOperands operands = intercalate ", " $ map printOperand operands

-------------------------------------------------------------------------------
-- * Gate Printing.

-- | Takes as input a legacy flag and a gate modifier. If the legacy flag is
-- false, then the gate modifier is converted recursively into a sequence of
-- modifier expressions (where the null modifier is the base case). Otherwise,
-- if the gate modifier is non-null, then an error is returned.
printMods :: Bool -> GateMod -> String
printMods _ (GateMod False []) = ""
printMods False (GateMod False (Pos:c)) = "ctrl @ " ++ rest
    where rest = printMods False (GateMod False c)
printMods False (GateMod False (Neg:c)) = "negctrl @ " ++ rest
    where rest = printMods False (GateMod False c)
printMods False (GateMod True c) = "inv @ " ++ rest
    where rest = printMods False (GateMod False c)
printMods True (GateMod True _) = toolError _INVS_TOOL line
    where line = "Inverse modifiers are not supported in legacy mode."
printMods True (GateMod False _) = toolError _CTRLS_TOOL line
    where line = "Control modifiers are not supported in legacy mode."

-- | Takes as input a gate name and a legacy flag. Returns the string
-- representation of the name. If the legacy flag is set, then an OpenQASM 2.0
-- compatible name is returned (often these names are the same).
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

-- | Takes as input a legacy flag and a gate. The gate is converted to its
-- syntactic representation. If the legacy flag is false, then modifiers are
-- expanded. Otherwise, if modifiers are provided, then an error is raised.
printGate :: Bool -> Gate -> String
printGate legacy (NamedGate name params operands mods) = mstr ++ body
    where mstr = printMods legacy mods 
          nstr = printName legacy name
          pstr = printParams legacy "" params
          ostr = printOperands operands
          body = nstr ++ pstr ++ " " ++ ostr
printGate legacy (GPhaseGate param operands mods) = mstr ++ body
    where mstr = printMods legacy mods 
          pstr = printParams legacy "" [param]
          ostr = printOperands operands
          body = "gphase" ++ pstr ++ " " ++ ostr

-------------------------------------------------------------------------------
-- * LValue and RValue Printing.

-- | Takes as input a legacy flag, the name of a declaration, and optionally an
-- index into the declaration (e.g., if the declaration is an array). Returns a
-- textual representation of the corresponding lvalue, adhering to the legacy
-- flag.
printLValue :: String -> Maybe Int -> String
printLValue id Nothing      = printOperand $ QRef id
printLValue id (Just index) = printOperand $ Cell id index

-- | Takes as input a legacy flag and an rvalue. Returns a textual
-- representation of the rvalue, adhering to the legacy flag.
printRValue :: Bool -> RValue -> String
printRValue legacy (QuipMeasure op) = printQHelper legacy "QMeas" op
printRValue legacy QuipCInit0       = printCHelper legacy "CInit0"
printRValue legacy QuipCInit1       = printCHelper legacy "CInit1"
printRValue legacy QuipCTerm0       = printCHelper legacy "CTerm0"
printRValue legacy QuipCTerm1       = printCHelper legacy "CTerm1"
printRValue legacy QuipCDiscard     = printCHelper legacy "CDiscard"
printRValue _      (Measure op)     = "measure " ++ printOperand op

-------------------------------------------------------------------------------
-- * Statement Printing.

-- | Takes as input the length associated with a variable declaration. If the
-- length is just an integer, the the corresponding designator string is
-- returned. Otherwise, nothing is returned.
printArrLen :: Maybe Int -> String
printArrLen Nothing  = ""
printArrLen (Just n) = "[" ++ show n ++ "]"

-- | Takes as input a legacy flag, a legacy declaration keyword (e.g., qreg), a
-- current declaration keyword (e.g., qubit), an array length (len), and a
-- declaration name (decl). If the legacy flag is set, then a legacy
-- declaration of the form `kw[len] decl;` is returned, where the designator is
-- optional. Otherwise, a declaration of the form `kw decl[len];` is returned,
-- where the designator is also optional.
printDecl :: Bool -> String -> String -> Maybe Int -> String -> String
printDecl True   kw _  len decl = kw ++ " " ++ decl ++ printArrLen len ++ ";"
printDecl False  _  kw len decl = kw ++ printArrLen len ++ " " ++ decl ++ ";"

-- | Takes as input a legacy flag, array length (len), and qubit declaration
-- name (decl). If the legacy flag is false, then a qubit declaration is
-- returned with the designator corresponding to len. Otherwise, a qreg
-- declaration is returned using the same len and decl.
printQubitDecl :: Bool -> Maybe Int -> String -> String
printQubitDecl legacy len decl = printDecl legacy "qreg" "qubit" len decl

-- | Takes as input a legacy flag, array length (len), and classical bit
-- declaration name (decl). If the legacy flag is flase, then a classical bit
-- declaration is returned with the designator corresponding to len. Otherwise,
-- a creg declaration is returned using the same len and decl.
printBitDecl :: Bool -> Maybe Int -> String -> String
printBitDecl legacy len decl = printDecl legacy "creg" "bit" len decl

-- | Takes as input a legacy flag, the name of a declaration, optionally an
-- index into the declaration (e.g., if the declaration is an array), and an
-- rvalue. Returns a textual representation of assigning the rvalue to the
-- given index of the declaration, adhering to the legacy flag.
printAssign :: Bool -> String -> Maybe Int -> RValue -> String
printAssign True id index rval@(Measure op) = rstr ++ " -> " ++ lstr ++ ";"
    where lstr = printLValue id index
          rstr = printRValue True rval
printAssign True _ _ rval
    | rstr == "" = error "UNREACHABLE!"
    | otherwise  = error "Assignments are not supported in legacy mode."
    where rstr = printRValue True rval
printAssign legacy id index rval = lstr ++ " = " ++ rstr ++ ";"
    where lstr = printLValue id index
          rstr = printRValue legacy rval

-- | Takes as input a legacy flag and an instance of a void call. Returns a
-- textual representation of the void call, adhering to the legacy flag.
printCall :: Bool -> VoidCall -> String
printCall legacy (QuipQInit0 op)   = printQHelper legacy "QInit0" op ++ ";"
printCall legacy (QuipQInit1 op)   = printQHelper legacy "QInit1" op ++ ";"
printCall legacy (QuipQTerm0 op)   = printQHelper legacy "QTerm0" op ++ ";"
printCall legacy (QuipQTerm1 op)   = printQHelper legacy "QTerm1" op ++ ";"
printCall legacy (QuipQDiscard op) = printQHelper legacy "QDiscard" op ++ ";"
printCall _      (VoidReset op)    = "reset " ++ printOperand op ++ ";"
printCall _      (VoidMeasure op)  = "measure " ++ printOperand op ++ ";"

-- | Concretizes a statement, and produces its syntactic representation.
printAstStmt :: Bool -> AstStmt -> String
printAstStmt legacy (AstGateStmt n gate)
    | n == 0     = gateStr ++ ";"
    | not legacy = "pow(" ++ show n ++ ") @ " ++ gateStr ++ ";"
    | otherwise  = toolError _POWS_TOOL errline
    where gateStr = printGate legacy gate
          errline = "Power modifiers are not supported in legacy mode."
printAstStmt legacy (AstQubitDecl len decl)   = printQubitDecl legacy len decl
printAstStmt legacy (AstBitDecl len decl)     = printBitDecl legacy len decl
printAstStmt legacy (AstAssign id index rval) = printAssign legacy id index rval
printAstStmt legacy (AstCall call)            = printCall legacy call

-- | Concretizes each statement, and produces its syntactic representation.
printAst :: Bool -> [AstStmt] -> [String]
printAst legacy = map $ printAstStmt legacy
