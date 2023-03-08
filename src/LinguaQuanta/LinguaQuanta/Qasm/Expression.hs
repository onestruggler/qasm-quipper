-- | Methods to simplify or intrepret expressions.

module LinguaQuanta.Qasm.Expression
  ( ExprErr(..)
  , applyBinaryOp
  , applyUnaryOp
  , avgExpr
  , euler
  , negateExpr
  , parseGateOperand
  , readDecInt
  , readFloat
  , toArrayIndex
  , toConstFloat
  , toConstInt
  , toQasm3
  , toRValue
  , toVoidCall
  , zero
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Either (expandLeft)
import LinguaQuanta.List (splitAtFirst)
import LinguaQuanta.Qasm.Language
  ( Expr
  , GateOperand
  )
import LinguaQuanta.Qasm.Language as Qasm
import LinguaQuanta.Qasm.Operand
  ( Operand(..)
  , RValue(..)
  , VoidCall(..)
  )
import Quantum.Synthesis.SymReal
  ( SymReal
  , to_real
  )
import Quantum.Synthesis.SymReal as SR

-------------------------------------------------------------------------------
-- * Evaluation and Error Handling Types.

data ExprErr = BadType String
             | NonConstId String
             | NegIntExp Int
             | UnknownCall String Int
             | UnknownRValue
             | CallArityMismatch String Int Int
             | NegArrIdx Int
             | NonOperandExpr
             | UnexpectedMeasure
             deriving (Show, Eq)

type ExprEval a   = Either a ExprErr
type EvalFn a err = Expr -> Either a err
type ExprEvalFn a = EvalFn a ExprErr

-------------------------------------------------------------------------------
-- * Useful Expressions.

-- | The literal zero.
zero :: Expr
zero = Qasm.DecInt "0"

-- | An approximation of the literal e.
euler :: Expr
euler = DecFloat $ show v
    where v = to_real SR.Euler :: Double

-------------------------------------------------------------------------------
-- * Evaluation Helper Methods.

-- | Converts an OpenQASM 3 decimal integer string to an integer value. Recall
-- that underscores may appear in decimal integer literals as padding.
readDecInt :: String -> Int
readDecInt = read . (filter (/= '_'))

-- | Takes as input an OpenQASM 3 float string. Returns an equivalent OpenQASM
-- float string of either the form `INT EXP`, or `INT.INT`, or `INT.INT EXP`.
-- Note that such strings are also valid Haskell doubles.
padFloat :: String -> String
padFloat str =
    case splitAtFirst (== '.') str of
        (istr, "")    -> istr
        (qpart, rest) -> case splitAtFirst isExp rest of
            (rpart, "")  -> mergeParts qpart rpart ""
            (rpart, exp) -> mergeParts qpart rpart $ "e" ++ exp
    where isExp c          = c == 'e' || c == 'E'
          handleNull str   = if null str then "0" else str
          mergeParts q r e = (handleNull q) ++ "." ++ (handleNull r) ++ e

-- | Converts an OpenQASM 3 float string to a double value. Recall that
-- underscores may appear in the whole, decimal, or expoential component of a
-- float literal as padding.
readFloat :: String -> Double
readFloat = read . padFloat . (filter (/= '_'))

-- | Consumes an evaluation function (f), a binary operation on the evaluation
-- type (op), and two expressions (lhs and rhs). If (f lhs) evaluates to v1 and
-- (f rhs) evaluates to v2, then returns (op v1 v2). Otherwise, returns the
-- first error value produced by f.
applyBinaryOp :: EvalFn a err -> (a -> a -> b) -> Expr -> Expr -> Either b err
applyBinaryOp f op lhs rhs =
    expandLeft (f lhs) $
        \x -> expandLeft (f rhs) $
            \y -> Left $ op x y

-- | Consumes an evaluation function (f), a unary operation on the evaluation
-- type (op), and an expression (expr). If (f expr) evaluates to v, then
-- returns (op v). Otherwise, returns the first error value produced by f.
applyUnaryOp :: EvalFn a err -> (a -> b) -> Expr -> Either b err
applyUnaryOp f op expr = expandLeft (f expr) $ \x -> Left $ op x

-- | Consumes the name of a function (id), an evaluation function (f), a unary
-- operation on the evaluation type (op), and a list of arguments (args). If
-- args has length 1 and (f $ head args) evalues to v, then returns (op v).
-- Otherwise, returns the first error value produced by f.
applyUnaryFn :: String -> ExprEvalFn a -> (a -> b) -> [Expr] -> ExprEval b
applyUnaryFn _  f call [arg] = applyUnaryOp f call arg
applyUnaryFn id _ _    args  = Right $ CallArityMismatch id actual 1
    where actual = length args

-- | Consumes the name of a function (id), an evaluation function (f), a binary
-- operation on the evaluation type (op), and a list of arguments (args). If
-- args has length 2, (f $ head args) evaluates to v1 and (f $ head $ tail args)
-- evaluates to v2, then returns (op v1 v2). Otherwise, returns the first error
-- value produced by f.
applyBinaryFn :: String -> ExprEvalFn a -> (a -> a -> b) -> [Expr] -> ExprEval b
applyBinaryFn _  f call [a1, a2] = applyBinaryOp f call a1 a2
applyBinaryFn id _ _    args     = Right $ CallArityMismatch id actual 2
    where actual = length args

-------------------------------------------------------------------------------
-- * Abstraction Methods.

-- | Evaluates an expression as a an array index (i.e., a non-negative const
-- integer). If the evaluation is possible, then the corresponding integer is
-- returned. Otherwise, returns an error describing the first failure.
toArrayIndex :: Expr -> ExprEval Int
toArrayIndex expr = expandLeft (toConstInt expr) $
    \n -> if n >= 0 then Left n else Right $ NegArrIdx n

-- | Evaluates an expression as a quantum gate operand. If the evaluation is
-- possible, then the corresponding integer is returned. Otherwise, returns an
-- error describing the first failure.
toOperand :: Expr -> ExprEval Operand
toOperand (Brack expr)            = toOperand expr
toOperand (Qasm.QasmId str)       = Left $ QRef str
toOperand (Qasm.QasmCell str idx) = expandLeft (toArrayIndex idx) $
                                               \n -> Left $ Cell str n
toOperand _ = Right NonOperandExpr

-- | Takes as input a gate operand (op). If op is a QVar, then a QRef operand
-- is returned. If op is a QReg with constant index, then a Cell operand is
-- returned. Otherwise, a gate summarization is returned to explain why the
-- array cell index is non-constant.
parseGateOperand :: GateOperand -> ExprEval Operand
parseGateOperand (QVar id) = Left $ QRef id
parseGateOperand (QReg id idx) = expandLeft (toArrayIndex idx) $
                                            \n -> Left $ Cell id n

-- | Takes as input a function name, a unary call of input type Operand, and a
-- list of arguments. If the argument list evaluates to a single operand, then
-- the unary call is applied to given operand with the bound call returned.
-- Otherwise, returns an error describing the first failure.
operandToCall :: String -> (Operand -> a) -> [Expr] -> ExprEval a
operandToCall name ctor [arg] = expandLeft (toOperand arg) $
                                           \op -> Left $ ctor op
operandToCall name _ args = Right $ CallArityMismatch name actual 1
    where actual = length args

-- | Takes as input a function name, a nullary call, and a list of arguments.
-- If the argument list is empty, then the nullary call is returned. Otherwise,
-- a CallArityMismatch error is returned.
toNullaryCall :: String -> a -> [Expr] -> ExprEval a
toNullaryCall name call []   = Left call
toNullaryCall name _    args = Right $ CallArityMismatch name actual 0
    where actual = length args

-- | Takes as input the argument to a measurement call. If the argument can be
-- evaluated as an operand, then the corresponding measurement call is
-- returned. Otherwise, nothing is returned.
toMeasure :: GateOperand -> ExprEval RValue
toMeasure expr = expandLeft (parseGateOperand expr) $
                            \op -> Left $ Measure op

-- | Evaluates an expression as an r-value (i.e., a valid expression for the
-- right-hand side of an assignment operation). If the evaluation is possible,
-- then the corresponding RValue is returned. Otherwise, returns an error
-- describing the first failure.
toRValue :: Expr -> ExprEval RValue
toRValue (Brack expr)                = toRValue expr
toRValue (Call name@"QMeas" args)    = operandToCall name QuipMeasure  args
toRValue (Call name@"CInit0" args)   = toNullaryCall name QuipCInit0   args
toRValue (Call name@"CInit1" args)   = toNullaryCall name QuipCInit1   args
toRValue (Call name@"CTerm0" args)   = toNullaryCall name QuipCTerm0   args
toRValue (Call name@"CTerm1" args)   = toNullaryCall name QuipCTerm1   args
toRValue (Call name@"CDiscard" args) = toNullaryCall name QuipCDiscard args
toRValue (QasmMeasure arg)           = toMeasure arg
toRValue _                           = Right UnknownRValue

-- | Takes as input a string (name) and a list of expressions (args). Evaluates
-- the inputs as a expression (Call name args) of void return type. If the
-- evaluation is possible, then the corresponding VoidCall is returned.
-- Otherwise, returns an error describing the first failure.
toVoidCall :: String -> [Expr] -> ExprEval VoidCall
toVoidCall name@"QInit0"   args = operandToCall name QuipQInit0   args
toVoidCall name@"QInit1"   args = operandToCall name QuipQInit1   args
toVoidCall name@"QTerm0"   args = operandToCall name QuipQTerm0   args
toVoidCall name@"QTerm1"   args = operandToCall name QuipQTerm1   args
toVoidCall name@"QDiscard" args = operandToCall name QuipQDiscard args
toVoidCall name            args = Right $ UnknownCall name $ length args

-------------------------------------------------------------------------------
-- * Evaluation Methods.

-- | Evaluates a function call as a constant integer literal. If the evaluation
-- is possible, then the corresponding integer is returned. Otherwise, returns
-- an error describing the first failure.
callToConstInt :: String -> [Expr] -> ExprEval Int
callToConstInt name args
    | name == "mod" = applyBinaryFn name toConstInt rem args
    | name == "pow" = case args of
        [arg1, arg2] -> expandLeft (toConstInt arg1) $
            \a -> expandLeft (toConstInt arg2) $
                \b -> if b < 0
                      then Right $ NegIntExp b
                      else Left $ a^b
        _ -> Right $ CallArityMismatch name actual 2
    | otherwise = Right $ UnknownCall name actual
    where actual = length args

-- | Evaluates an expression as a constant integer literal. If the evaluation
-- is possible, then the corresponding integer is returned. Otherwise, returns
-- an error describing the first failure.
toConstInt :: Expr -> ExprEval Int
toConstInt (Qasm.Plus lhs rhs)   = applyBinaryOp toConstInt (+) lhs rhs
toConstInt (Qasm.Minus lhs rhs)  = applyBinaryOp toConstInt (-) lhs rhs
toConstInt (Qasm.Times lhs rhs)  = applyBinaryOp toConstInt (*) lhs rhs
toConstInt (Qasm.Div lhs rhs)    = applyBinaryOp toConstInt div lhs rhs
toConstInt (Qasm.Brack expr)     = toConstInt expr
toConstInt (Qasm.Negate expr)    = applyUnaryOp toConstInt (\x -> -x) expr
toConstInt (Qasm.Call str args)  = callToConstInt str args
toConstInt Qasm.Euler            = Right $ BadType "float"
toConstInt Qasm.Pi               = Right $ BadType "angle"
toConstInt Qasm.Tau              = Right $ BadType "angle"
toConstInt (Qasm.DecFloat _)     = Right $ BadType "float"
toConstInt (Qasm.DecInt str)     = Left $ readDecInt str
toConstInt (Qasm.QasmId str)     = Right $ NonConstId str
toConstInt (Qasm.QasmCell str _) = Right $ NonConstId str
toConstInt (QasmMeasure _)       = Right $ UnexpectedMeasure

-- | Converts an OpenQASM function call to a SymReal expression. If the
-- conversion is possible, then the corresponding symbolic real is returned.
-- Otherwise, returns an error describing the first failure.
--
-- Note: Conversion to symbolic reals requries compile-time integers or floats.
callToSymReal :: String -> [Expr] -> ExprEval SymReal
callToSymReal name args
    | name == "arccos"  = applyUnaryFn  name toSymReal    SR.ACos   args
    | name == "arcsin"  = applyUnaryFn  name toSymReal    SR.ASin   args
    | name == "arctan"  = applyUnaryFn  name toSymReal    SR.ATan   args
    | name == "cos"     = applyUnaryFn  name toSymReal    SR.Cos    args
    | name == "sin"     = applyUnaryFn  name toSymReal    SR.Sin    args
    | name == "tan"     = applyUnaryFn  name toSymReal    SR.Tan    args
    | name == "exp"     = applyUnaryFn  name toSymReal    SR.Exp    args
    | name == "sqrt"    = applyUnaryFn  name toSymReal    SR.Sqrt   args
    | name == "log"     = applyUnaryFn  name toSymReal    SR.Log    args
    | name == "pow"     = applyBinaryFn name toSymReal    SR.Power  args
    | name == "ceiling" = applyUnaryFn  name toConstFloat srCeiling args
    | name == "floor"   = applyUnaryFn  name toConstFloat srFloor   args
    | name == "mod"     = applyBinaryFn name toConstFloat srMod     args
    | otherwise         = Right $ UnknownCall name $ length args
    where srCeiling  = SR.Const . ceiling
          srFloor    = SR.Const . floor
          srMod a b  = SR.Const $ rem (floor a) (floor b)

-- | Converts an OpenQASM expression to a SymReal expression. If the conversion
-- is possible, then the corresponding symbolic real is returned. Otherwise,
-- returns an error describing the first failure.
--
-- Note: Conversion to symbolic reals requries compile-time integers or floats.
toSymReal :: Expr -> ExprEval SymReal
toSymReal (Qasm.Plus lhs rhs)   = applyBinaryOp toSymReal SR.Plus lhs rhs
toSymReal (Qasm.Minus lhs rhs)  = applyBinaryOp toSymReal SR.Minus lhs rhs
toSymReal (Qasm.Times lhs rhs)  = applyBinaryOp toSymReal SR.Times lhs rhs
toSymReal (Qasm.Div lhs rhs)    = applyBinaryOp toSymReal SR.Div lhs rhs
toSymReal (Qasm.Brack expr)     = toSymReal expr
toSymReal (Qasm.Negate expr)    = applyUnaryOp toSymReal SR.Negate expr
toSymReal (Qasm.Call str args)  = callToSymReal str args
toSymReal Qasm.Euler            = Left $ SR.Euler
toSymReal Qasm.Pi               = Left $ SR.Pi
toSymReal Qasm.Tau              = Left $ SR.Times SR.Pi $ SR.Const 2
toSymReal (Qasm.DecInt str)     = Left $ SR.Const $ toInteger $ readDecInt str
toSymReal (Qasm.QasmId str)     = Right $ NonConstId str
toSymReal (Qasm.QasmCell str _) = Right $ NonConstId str
toSymReal (QasmMeasure _)       = Right $ UnexpectedMeasure
toSymReal (Qasm.DecFloat str)   = Left $ SR.Decimal val str
    where val = toRational $ readFloat str

-- | Evaluates an expression as a constant double literal. If the evaluation
-- is possible, then the corresponding double is returned. Otherwise, returns
-- an error describing the first failure.
toConstFloat :: Expr -> ExprEval Double
toConstFloat expr = expandLeft (toSymReal expr) $
                               \val -> Left $ to_real val


-- | Takes as input an OpenQASM 2.0 expression. Returns an equivalent OpenQASM
-- 3 expression. In particular, every instance of ln is replaced by log.
toQasm3 :: Expr -> Expr
toQasm3 (Qasm.Plus lhs rhs)      = Qasm.Plus (toQasm3 lhs) (toQasm3 rhs)
toQasm3 (Qasm.Minus lhs rhs)     = Qasm.Minus (toQasm3 lhs) (toQasm3 rhs)
toQasm3 (Qasm.Times lhs rhs)     = Qasm.Times (toQasm3 lhs) (toQasm3 rhs)
toQasm3 (Qasm.Div lhs rhs)       = Qasm.Div (toQasm3 lhs) (toQasm3 rhs)
toQasm3 (Qasm.Brack expr)        = Qasm.Brack $ toQasm3 expr
toQasm3 (Qasm.Negate expr)       = Qasm.Negate $ toQasm3 expr
toQasm3 Qasm.Euler               = Qasm.Euler
toQasm3 Qasm.Pi                  = Qasm.Pi
toQasm3 Qasm.Tau                 = Qasm.Tau
toQasm3 lit@(Qasm.DecInt _)      = lit
toQasm3 lit@(Qasm.DecFloat _)    = lit
toQasm3 id@(Qasm.QasmId _)       = id
toQasm3 cell@(Qasm.QasmCell _ _) = cell
toQasm3 expr@(QasmMeasure _)     = expr
toQasm3 (Qasm.Call name args)    = Qasm.Call name' args'
    where name' = if name == "ln" then "log" else name
          args' = map toQasm3 args

-------------------------------------------------------------------------------
-- * Manipulation Methods.

-- | Returns the symbolic negation of a numeric expression.
negateExpr :: Expr -> Expr
negateExpr expr = Qasm.Negate (Qasm.Brack expr)

-- | Returns the symbolic average of two numeric expressions.
avgExpr :: Expr -> Expr -> Expr
avgExpr lhs rhs = Qasm.Div (Qasm.Brack (Qasm.Plus lhs rhs)) (Qasm.DecInt "2")
