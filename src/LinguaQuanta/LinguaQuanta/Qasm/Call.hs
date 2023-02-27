-- | Functions for analzing and inlining function calls.

module LinguaQuanta.Qasm.Call
  ( elimCallsInArglist
  , elimCallsInExpr
  , isLegacyCall
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.Expression
  ( toConstFloat
  , toConstInt
  )
import LinguaQuanta.Qasm.Language (Expr(..))

-------------------------------------------------------------------------------
-- * Call Analysis.

-- | Returns true if a call name is built into OpenQASM 2.0.
isLegacyCall :: String -> Bool
isLegacyCall "sin"  = True
isLegacyCall "cos"  = True
isLegacyCall "tan"  = True
isLegacyCall "exp"  = True
isLegacyCall "ln"   = True
isLegacyCall "sqrt" = True
isLegacyCall _      = False

-------------------------------------------------------------------------------
-- * Call Inlining.

-- | Encodes either an OpenQASM expression, or the name of a call.
type ExprOrCall = Either Expr String

-- | Takes as input a binary operator, together with its left-hand side and
-- right-hand side. Applies elimCallsInExpr to both the left-hand side and
-- right-hand side. If both calls are successful, then the operator is applied
-- to the new left-hand side and the new right-hand side. Otherwise, the first
-- error value is returned.
elimCallsInBinaryOp :: (Expr -> Expr -> Expr) -> Expr -> Expr -> ExprOrCall
elimCallsInBinaryOp op lhs rhs =
    case elimCallsInExpr lhs of
        Left lhs' -> case elimCallsInExpr rhs of
            Left rhs'  -> Left $ op lhs' rhs'
            Right name -> Right name
        Right name -> Right name

-- | Takes as input a unary operator, together with its argument. If applying
-- elimCallsInExpr to the argument yields a new expression, then the operator
-- is applied to this new expression. Otherwise, the error value is returned.
elimCallsInUnary :: (Expr -> Expr) -> Expr -> ExprOrCall
elimCallsInUnary op arg =
    case elimCallsInExpr arg of
        Left arg'  -> Left $ op arg'
        Right name -> Right name

-- | Takes as input the name and arguments of a function call. If the function
-- is built into OpenQASM 2.0, then elimCallsInArglist is applied to the
-- argument list where either elimCallsInArglist fails and the error value is
-- returned, or elimCallsInArglist succeeds and the new call is returned. If
-- the function is not built into OpenQASM 2.0, then an attempt is made to
-- evaluate the call, first as an integer, and then as a float. If both
-- attempts fail, then the name of the call is returned.
elimCall :: String -> [Expr] -> ExprOrCall
elimCall name args =
    if isLegacyCall name
    then case elimCallsInArglist args of
        Left args' -> Left $ Call name args'
        Right err  -> Right err
    else case toConstInt call of
        Left val -> Left $ DecInt $ show val
        Right _  -> case toConstFloat call of
            Left val -> Left $ DecFloat $ show val
            Right _  -> Right name
    where call = Call name args

-- | Takes as input an OpenQASM expression. If all functions not built into
-- OpenQASM 2.0 can be inlined, then this new expression is returned.
-- Otherwise, returns the name of the first function failed to be inlined.
elimCallsInExpr :: Expr -> ExprOrCall
elimCallsInExpr (Plus lhs rhs)    = elimCallsInBinaryOp Plus lhs rhs
elimCallsInExpr (Minus lhs rhs)   = elimCallsInBinaryOp Minus lhs rhs
elimCallsInExpr (Times lhs rhs)   = elimCallsInBinaryOp Times lhs rhs
elimCallsInExpr (Div lhs rhs)     = elimCallsInBinaryOp Div lhs rhs
elimCallsInExpr (Brack expr)      = elimCallsInUnary Brack expr
elimCallsInExpr (Negate expr)     = elimCallsInUnary Negate expr
elimCallsInExpr Euler             = Left Euler
elimCallsInExpr Pi                = Left Pi
elimCallsInExpr Tau               = Left Tau
elimCallsInExpr lit@(DecFloat _)  = Left lit
elimCallsInExpr lit@(DecInt _)    = Left lit
elimCallsInExpr id@(QasmId _)     = Left id
elimCallsInExpr (Call name args)  = elimCall name args
elimCallsInExpr (QasmCell id idx) =
    case elimCallsInExpr idx of
        Left idx' -> Left $ QasmCell id idx'
        Right err -> Right err

-- | Applies elimCallsInExpr to the elements of an argument list. If any
-- application returns an error value, then the error value is returned.
-- Otherwise, returns a list equivalent to mapping elimCallsInExpr over all
-- elements of the input list.
elimCallsInArglist :: [Expr] -> Either [Expr] String
elimCallsInArglist []         = Left []
elimCallsInArglist (arg:args) =
    case elimCallsInExpr arg of
        Left arg' -> case elimCallsInArglist args of
            Left args' -> Left $ arg':args'
            Right err  -> Right err
        Right err -> Right err
