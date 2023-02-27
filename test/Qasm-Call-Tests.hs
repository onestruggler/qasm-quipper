module Main where

import Data.Either
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.Expression
import LinguaQuanta.Qasm.Language
import LinguaQuanta.Qasm.Call

-----------------------------------------------------------------------------------------
-- * elimCallsInExpr

makeExpr :: Expr -> Expr -> Expr
makeExpr t1 t2 = Div lhs (Plus rhs t2)
    where lhs = Negate $ Plus Pi $ Times Euler $ Minus Tau t1
          rhs = Brack $ Plus int $ Plus dbl $ Plus (QasmId "v") $ QasmCell "q" int
          int = DecInt "5"
          dbl = DecFloat "2.5"

test1 = TestCase (assertEqual "elimCallsInExpr leaves legacy calls unchanged."
                              (Left $ makeExpr t1 t2 :: Either Expr String)
                              (elimCallsInExpr $ makeExpr t1 t2))
    where t1 = Call "sin" [Pi]
          t2 = Call "exp" [Euler]

test2 = TestCase (assertEqual "elimCallsInExpr evaluates non-legacy calls."
                              (Left $ makeExpr t1b t2b :: Either Expr String)
                              (elimCallsInExpr $ makeExpr t1a t2a))
    where nc1 = Call "arcsin" [DecInt "1"]
          nc2 = Call "mod" [DecInt "2", DecInt "5"]
          t1a = Call "sin" [nc1]
          t2a = Call "exp" [nc2]
          t1b = Call "sin" [DecFloat $ show $ fromLeft 0 $ toConstFloat nc1]
          t2b = Call "exp" [DecInt $ show $ fromLeft 0 $ toConstInt nc2]

test3 = TestCase (assertEqual "elimCallsInExpr handles evaluation failures."
                              (Right "arcsin" :: Either Expr String)
                              (elimCallsInExpr $ makeExpr t1 t2))
    where t1 = Call "sin" [Pi]
          t2 = Call "arcsin" [QasmCell "v" $ DecInt "5"]

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "elimCallsInExpr_Unchanged" test1,
                                     TestLabel "elimCallsInExpr_Inlining" test2,
                                     TestLabel "elimCallsInExpr_EvalFailure" test3]

main = defaultMain tests
