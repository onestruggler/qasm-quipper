-- | Methods to simplify or intrepret expressions.

module Qasm.Expression
  ( ExprErr(..)
  , readDecInt
  , toConstInt
  ) where

import Qasm.Language (Expr(..))

-------------------------------------------------------------------------------
-- * Evaluation and Error Handling Types.

data ExprErr = BadType String
             | NonConstId String
             deriving (Show, Eq)

type ExprEval a = Either a ExprErr
type ExprEvalFn a = Expr -> ExprEval a

-------------------------------------------------------------------------------
-- * Evaluation Helper Methods.

-- | Converts an OpenQASM 3 decimal integer string to an integer value. Recall
-- that underscores may appear in decimal integer literals as padding.
readDecInt :: String -> Int
readDecInt = read . (filter (/= '_'))

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

-- Evaluates an expression as a constant integer literal. If the evaluation is
-- possible, then the corresponding integer is returned. Otherwise, returns an
-- error describing the first failure.
toConstInt :: Expr -> ExprEval Int
toConstInt (Plus lhs rhs)  = applyBinaryOp toConstInt (+) lhs rhs 
toConstInt (Minus lhs rhs) = applyBinaryOp toConstInt (-) lhs rhs
toConstInt (Times lhs rhs) = applyBinaryOp toConstInt (*) lhs rhs
toConstInt (Div lhs rhs)   = applyBinaryOp toConstInt div lhs rhs
toConstInt (Brack expr)    = toConstInt expr
toConstInt (Negate expr)   = applyUnaryOp toConstInt (\x -> -x) expr
toConstInt Pi              = Right (BadType "angle")
toConstInt (DecInt str)    = Left (readDecInt str)
toConstInt (QasmId str)    = Right (NonConstId str)
