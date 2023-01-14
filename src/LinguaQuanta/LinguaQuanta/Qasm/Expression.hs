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
applyBinaryOp :: ExprEvalFn a -> (a -> a -> a) -> Expr -> Expr -> ExprEval a
applyBinaryOp f op lhs rhs =
    case f lhs of
        Left x -> case f rhs of
            Left y  -> Left (op x y)
            Right e -> Right e
        Right e -> Right e

-- | Consumes an evaluation function (f), a unary operation on the evaluation
-- type (op), and an expression (expr). If (f expr) evaluates to v, then
-- returns (op v). Otherwise, returns the first error value produced by f.
applyUnaryOp :: ExprEvalFn a -> (a -> a) -> Expr -> ExprEval a
applyUnaryOp f op expr =
    case f expr of
        Left x  -> Left (op x)
        Right e -> Right e

-------------------------------------------------------------------------------
-- * Evaluation Methods.

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
toConstInt Qasm.Pi              = Right $ BadType "angle"
toConstInt (Qasm.DecFloat _)    = Right $ BadType "float"
toConstInt (Qasm.DecInt str)    = Left $ readDecInt str
toConstInt (Qasm.QasmId str)    = Right $ NonConstId str

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
toSymReal Qasm.Pi              = Left $ SR.Pi
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
