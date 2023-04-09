-- | Printing functions for the OpenQASM 3 AST.

module LinguaQuanta.Qasm.Printer (printAst) where

-------------------------------------------------------------------------------
-- * Import Section.

import Data.List (intercalate)
import LinguaQuanta.Qasm.AST (AstStmt(..))
import LinguaQuanta.Qasm.Call (isLegacyCall)
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
import LinguaQuanta.Qasm.GateName
  ( GateName(..)
  , isBackportGate
  , isQelib1Gate
  , isQuipperGate
  )
import LinguaQuanta.Qasm.Language (Expr(..))
import LinguaQuanta.Qasm.Operand
  ( Operand(..)
  , RValue(..)
  , VoidCall(..)
  )

-------------------------------------------------------------------------------
-- * Library Usage Summarization.

-- | A data structure to track the built-in OpenQASM libraries used by the
-- program. The libraries are encoded in the following order: qelib1.inc,
-- bkpgates.inc, quipgates.inc, quipfuncs.inc.
data LibUsage = LibUsage Bool Bool Bool Bool deriving (Show, Eq)

-- | Associates a line of an OpenQASM program, together with its usage data.
type LineSummary = (LibUsage, String)

-- | Returns a LibUsage summary with no libraries in use.
depfree :: LibUsage
depfree = LibUsage False False False False

-- | Adds qelib1.inc to the list of libraries in use (in OpenQASM 3, this
-- corresponds to stdgates.inc).
useQelib1 :: LibUsage -> LibUsage
useQelib1 (LibUsage _ bkp qpr qfn) = LibUsage True bkp qpr qfn

-- | Adds bkpgates.inc to the list of libraries in use (in OpenQASM 3, this
-- corresponds to stdgates.inc).
useBkpgates :: LibUsage -> LibUsage
useBkpgates (LibUsage _ _ qpr qfn) = LibUsage True True qpr qfn

-- | Adds quipgates.inc to the list of libraries in use.
useQuipgates :: LibUsage -> LibUsage
useQuipgates (LibUsage _ bkp _ qfn) = LibUsage True bkp True qfn

-- | Adds quipfuncs.inc to the list of libraries in use.
useQuipfuncs :: LibUsage -> LibUsage
useQuipfuncs (LibUsage _ bkp qpr _) = LibUsage True bkp qpr True

-------------------------------------------------------------------------------
-- * Tools for Legacy Support.

_FUNS_TOOL  = "elim_funs"
_POWS_TOOL  = "elim_pows"
_INVS_TOOL  = "elim_invs"
_CTRLS_TOOL = "elim_ctrls"

-- | Produces an error and directs the user to a tool to resolve the error.
toolError :: String -> String -> a
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

-- | Prints build-in non-unitary quantum operators.
printBuiltinHelper :: String -> Operand -> String
printBuiltinHelper name op = name ++ " " ++ printOperand op

-------------------------------------------------------------------------------
-- * Expression Printing.

-- | Formates a binary operation as: lhs op rhs.
printBinaryOp :: Bool -> String -> Expr -> Expr -> String
printBinaryOp legacy op lhs rhs = lstr ++ " " ++ op ++ " " ++ rstr
    where lstr = printExpr legacy lhs
          rstr = printExpr legacy rhs

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
printExpr legacy (Call id args)    = printExprCall legacy id'  args
    where id' = if legacy && id == "log" then "ln" else id

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
printName GateU             = "U"
printName GateCU            = "cu"
printName GateU1            = "u1"
printName GateU2            = "u2"
printName GateU3            = "u3"

-- | Takes as input a library usage summary and a gate name. If the gate is
-- provided by a built-in library, then the library is added to the summary and
-- the new summary is returned. Otherwise, the unchanged summary is returned.
addGateToSummary :: LibUsage -> GateName -> LibUsage
addGateToSummary used name
    | isQelib1Gate name   = useQelib1 used
    | isBackportGate name = useBkpgates used
    | isQuipperGate name  = useQuipgates used
    | otherwise           = used

-- | Takes as input a legacy flag and a gate. The gate is converted to its
-- syntactic representation. If the legacy flag is false, then modifiers are
-- expanded. Otherwise, if modifiers are provided, then an error is raised.
printGate :: Bool -> LibUsage -> Gate -> LineSummary
printGate legacy used (NamedGate name params operands mods) = (used', line)
    where mstr  = printMods legacy mods 
          nstr  = printName name
          pstr  = printParams legacy "" params
          ostr  = printOperands operands
          body  = nstr ++ pstr ++ " " ++ ostr
          line  = mstr ++ body
          used' = addGateToSummary used name
printGate legacy used (GPhaseGate param operands mods) = (used, line)
    where mstr = printMods legacy mods
          pstr = printParams legacy "" [param]
          ostr = printOperands operands
          pref = if null ostr then "" else " " ++ ostr
          body = "gphase" ++ pstr ++ pref
          line = mstr ++ body

-- | Takes as input a legacy flag, a library usage summary, a string encoding
-- of a gate modifier, and a gate. Returns a syntactic representation of the
-- gate (with the given modifier applied), together with a new line summary
-- that takes into account the gate.
printGateStmt :: Bool -> LibUsage -> String -> Gate -> LineSummary
printGateStmt legacy used mod gate = (used', line)
    where (used', gatestr) = printGate legacy used gate
          line             = mod ++ gatestr ++ ";"

-------------------------------------------------------------------------------
-- * LValue and RValue Printing.

-- | Takes as input a legacy flag, the name of a declaration, and optionally an
-- index into the declaration (e.g., if the declaration is an array). Returns a
-- textual representation of the corresponding lvalue, adhering to the legacy
-- flag.
printLValue :: String -> Maybe Int -> String
printLValue id Nothing    = printOperand $ QRef id
printLValue id (Just idx) = printOperand $ Cell id idx

-- | Specializes printQHelper to printRValue.
printQRv :: Bool -> LibUsage -> String -> Operand -> LineSummary
printQRv legacy used name op = (used', call)
    where call  = printQHelper legacy name op
          used' = useQuipfuncs used

-- | Specializes printCHelper to printRValue.
printCRv :: Bool -> LibUsage -> String -> LineSummary
printCRv legacy used name = (used', call)
    where call  = printCHelper legacy name
          used' = useQuipfuncs used

-- | Specializes printBuiltinHelper to printRValue.
printBuiltinRv :: LibUsage -> String -> Operand -> LineSummary
printBuiltinRv used name op = (used, call)
    where call = printBuiltinHelper name op

-- | Takes as input a legacy flag and an rvalue. Returns a textual
-- representation of the rvalue, adhering to the legacy flag.
printRValue :: Bool -> LibUsage -> RValue -> LineSummary
printRValue legacy used (QuipMeasure op) = printQRv legacy used "QMeas" op
printRValue legacy used QuipCInit0       = printCRv legacy used "CInit0"
printRValue legacy used QuipCInit1       = printCRv legacy used "CInit1"
printRValue legacy used QuipCTerm0       = printCRv legacy used "CTerm0"
printRValue legacy used QuipCTerm1       = printCRv legacy used "CTerm1"
printRValue legacy used QuipCDiscard     = printCRv legacy used "CDiscard"
printRValue _      used (Measure op)     = printBuiltinRv used "measure" op

-------------------------------------------------------------------------------
-- * Call Printing.

-- | Specializes printQHelper to printCall.
printQCall :: Bool -> LibUsage -> String -> Operand -> LineSummary
printQCall legacy used name op = (used', call ++ ";")
    where call  = printQHelper legacy name op
          used' = useQuipfuncs used

-- | Specializes printBuiltinHelper to printCall.
printBuiltinCall :: LibUsage -> String -> Operand -> LineSummary
printBuiltinCall used name op = (used, call ++ ";")
    where call = printBuiltinHelper name op

-- | Takes as input a legacy flag and an instance of a void call. Returns a
-- textual representation of the void call, adhering to the legacy flag.
printCall :: Bool -> LibUsage -> VoidCall -> LineSummary
printCall legacy used (QuipQInit0 op)   = printQCall legacy used "QInit0" op
printCall legacy used (QuipQInit1 op)   = printQCall legacy used "QInit1" op
printCall legacy used (QuipQTerm0 op)   = printQCall legacy used "QTerm0" op
printCall legacy used (QuipQTerm1 op)   = printQCall legacy used "QTerm1" op
printCall legacy used (QuipQDiscard op) = printQCall legacy used "QDiscard" op
printCall _      used (VoidReset op)    = printBuiltinCall used "reset" op
printCall _      used (VoidMeasure op)  = printBuiltinCall used "measure" op

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
printAssign :: Bool -> LibUsage -> String -> Maybe Int -> RValue -> LineSummary
printAssign True used id idx rval@(Measure op) = (used', line)
    where lstr          = printLValue id idx
          (used', rstr) = printRValue True used rval
          line          = rstr ++ " -> " ++ lstr ++ ";"
printAssign True used _ _ rval
    | rstr == "" = error "UNREACHABLE!" -- Forces evaluation of printRValue.
    | otherwise  = error "Assignments are not supported in legacy mode."
    where (_, rstr) = printRValue True used rval
printAssign legacy used id idx rval = (used', line)
    where lstr          = printLValue id idx
          (used', rstr) = printRValue legacy used rval
          line          = lstr ++ " = " ++ rstr ++ ";"

-- | Concretizes a statement, and produces its syntactic representation.
printAstStmt :: Bool -> LibUsage -> AstStmt -> LineSummary
printAstStmt legacy used (AstGateStmt n gate)
    | n == 0     = printGateStmt legacy used "" gate
    | not legacy = printGateStmt legacy used pmodStr gate
    | otherwise  = toolError _POWS_TOOL errline
    where pmodStr = "pow(" ++ show n ++ ") @ "
          errline = "Power modifiers are not supported in legacy mode."
printAstStmt legacy used (AstQubitDecl len decl) = (used, line)
    where line = printQubitDecl legacy len decl
printAstStmt legacy used (AstBitDecl len decl) = (used, line)
    where line = printBitDecl legacy len decl
printAstStmt legacy used (AstAssign id idx rval) = res
    where res = printAssign legacy used id idx rval
printAstStmt legacy used (AstCall call) = printCall legacy used call

-- | Helper function which maps (printAstStmts legacy) through the list of
-- statements, while tracking the library usage at each step.
astToLines :: Bool -> [AstStmt] -> (LibUsage, [String])
astToLines _ []                = (depfree, [])
astToLines legacy (stmt:stmts) = (used', line:lines)
    where (used, lines) = astToLines legacy stmts
          (used', line) = printAstStmt legacy used stmt

-- | Returns the version string, according to whether the output is legacy.
printVersion :: Bool -> String
printVersion legacy = "OPENQASM " ++ vnum ++ ";"
    where vnum = if legacy then "2.0" else "3"

-- | Takes as input a boolean condition, a list of OpenQASM statements (encoded
-- as strings), and the name of an OpenQASM library. If the boolean condition
-- is satisfied, then preprends an include statement for the library to the
-- list. Otherwise, the list is returned unchaged.
addIncludeIf :: Bool -> [String] -> String -> [String]
addIncludeIf False lines _   = lines
addIncludeIf True  lines inc = line:lines
    where line = "include \"" ++ inc ++ "\";"

-- | Consumes the syntactic representation of an OpenQASM program, and prepends
-- an include statement for each built-in library used by the program.
prependIncludes :: Bool -> LibUsage -> [String] -> [String]
prependIncludes legacy (LibUsage std bkp qpr qfn) lines = tmp5
    where tmp0 = lines
          tmp1 = addIncludeIf qfn                          tmp0 "quipfuncs.inc"
          tmp2 = addIncludeIf qpr                          tmp1 "quipgates.inc"
          tmp3 = addIncludeIf ((std || bkp) && not legacy) tmp2 "stdgates.inc"
          tmp4 = addIncludeIf (bkp && legacy)              tmp3 "bkpgates.inc"
          tmp5 = addIncludeIf (std && legacy)              tmp4 "qelib1.inc"

-- | Concretizes each statement, and produces its syntactic representation.
printAst :: Bool -> [AstStmt] -> [String]
printAst legacy stmts = vers:bodyWithIncludes
    where (used, body)     = astToLines legacy stmts
          bodyWithIncludes = prependIncludes legacy used body
          vers             = printVersion legacy
