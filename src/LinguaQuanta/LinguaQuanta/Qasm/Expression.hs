-- | Methods to simplify or intrepret expressions.

module LinguaQuanta.Qasm.Expression
  ( ExprErr(..)
  , avgExpr
  , negateExpr
  , readDecInt
  , readFloat
  , toConstFloat
  , toConstInt
  , zero
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.List (splitAtFirst)
import LinguaQuanta.Qasm.Language (Expr)
import LinguaQuanta.Qasm.Language as Qasm
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
             | CallArityMismatch String Int Int
             deriving (Show, Eq)

type ExprEval a = Either a ExprErr
type ExprEvalFn a = Expr -> ExprEval a

-------------------------------------------------------------------------------
-- * Useful Expressions.

zero :: Expr
zero = Qasm.DecInt "0"

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
applyBinaryOp :: ExprEvalFn a -> (a -> a -> b) -> Expr -> Expr -> ExprEval b
applyBinaryOp f op lhs rhs =
    case f lhs of
        Left x -> case f rhs of
            Left y  -> Left $ op x y
            Right e -> Right e
        Right e -> Right e

-- | Consumes an evaluation function (f), a unary operation on the evaluation
-- type (op), and an expression (expr). If (f expr) evaluates to v, then
-- returns (op v). Otherwise, returns the first error value produced by f.
applyUnaryOp :: ExprEvalFn a -> (a -> b) -> Expr -> ExprEval b
applyUnaryOp f op expr =
    case f expr of
        Left x  -> Left $ op x
        Right e -> Right e

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
-- * Evaluation Methods.

-- | Evaluates a function call as a constant integer literal. If the evaluation
-- is possible, then the corresponding integer is returned. Otherwise, returns
-- an error describing the first failure.
callToConstInt :: String -> [Expr] -> ExprEval Int
callToConstInt name args
    | name == "mod" = applyBinaryFn name toConstInt rem args
    | name == "pow" = case args of
        [arg1, arg2] -> case toConstInt arg1 of
            Left a -> case toConstInt arg2 of
                Left b -> if b < 0
                          then Right $ NegIntExp b
                          else Left $ a^b
                Right e -> Right e
            Right e -> Right e
        _ -> Right $ CallArityMismatch name actual 2
    | otherwise = Right $ UnknownCall name actual
    where actual = length args

-- | Evaluates an expression as a constant integer literal. If the evaluation
-- is possible, then the corresponding integer is returned. Otherwise, returns
-- an error describing the first failure.
toConstInt :: Expr -> ExprEval Int
toConstInt (Qasm.Plus lhs rhs)  = applyBinaryOp toConstInt (+) lhs rhs 
toConstInt (Qasm.Minus lhs rhs) = applyBinaryOp toConstInt (-) lhs rhs
toConstInt (Qasm.Times lhs rhs) = applyBinaryOp toConstInt (*) lhs rhs
toConstInt (Qasm.Div lhs rhs)   = applyBinaryOp toConstInt div lhs rhs
toConstInt (Qasm.Brack expr)    = toConstInt expr
toConstInt (Qasm.Negate expr)   = applyUnaryOp toConstInt (\x -> -x) expr
toConstInt (Qasm.Call str args) = callToConstInt str args
toConstInt Qasm.Euler           = Right $ BadType "float"
toConstInt Qasm.Pi              = Right $ BadType "angle"
toConstInt Qasm.Tau             = Right $ BadType "angle"
toConstInt (Qasm.DecFloat _)    = Right $ BadType "float"
toConstInt (Qasm.DecInt str)    = Left $ readDecInt str
toConstInt (Qasm.QasmId str)    = Right $ NonConstId str

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
toSymReal (Qasm.Plus lhs rhs)  = applyBinaryOp toSymReal SR.Plus lhs rhs
toSymReal (Qasm.Minus lhs rhs) = applyBinaryOp toSymReal SR.Minus lhs rhs
toSymReal (Qasm.Times lhs rhs) = applyBinaryOp toSymReal SR.Times lhs rhs
toSymReal (Qasm.Div lhs rhs)   = applyBinaryOp toSymReal SR.Div lhs rhs
toSymReal (Qasm.Brack expr)    = toSymReal expr
toSymReal (Qasm.Negate expr)   = applyUnaryOp toSymReal SR.Negate expr
toSymReal (Qasm.Call str args) = callToSymReal str args
toSymReal Qasm.Euler           = Left $ SR.Euler
toSymReal Qasm.Pi              = Left $ SR.Pi
toSymReal Qasm.Tau             = Left $ SR.Times SR.Pi $ SR.Const 2
toSymReal (Qasm.DecInt str)    = Left $ SR.Const $ toInteger $ readDecInt str
toSymReal (Qasm.QasmId str)    = Right $ NonConstId str
toSymReal (Qasm.DecFloat str)  = Left $ SR.Decimal val str
    where val = toRational $ readFloat str

-- | Evaluates an expression as a constant double literal. If the evaluation
-- is possible, then the corresponding double is returned. Otherwise, returns
-- an error describing the first failure.
toConstFloat :: Expr -> ExprEval Double
toConstFloat expr =
    case toSymReal expr of
        Left val  -> Left $ to_real val
        Right err -> Right err

-------------------------------------------------------------------------------
-- * Manipulation Methods.

-- Returns the symbolic negation of a numeric expression.
negateExpr :: Expr -> Expr
negateExpr expr = Qasm.Negate (Qasm.Brack expr)

-- | Returns the symbolic average of two numeric expressions.
avgExpr :: Expr -> Expr -> Expr
avgExpr lhs rhs = Qasm.Div (Qasm.Brack (Qasm.Plus lhs rhs)) (Qasm.DecInt "2")
