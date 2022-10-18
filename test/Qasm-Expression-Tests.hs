module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Qasm.Expression
import Qasm.Language

-----------------------------------------------------------------------------------------
-- readDecInt

test1 = TestCase (assertEqual "readDecInt can read a positive integer."
                              10054
                              (readDecInt "10054"))

test2 = TestCase (assertEqual "readDecInt can read a positive integer with underscores."
                              24369
                              (readDecInt "2_4_3__6_9"))

test3 = TestCase (assertEqual "readDecInt can read a negative integer."
                              (-65390)
                              (readDecInt "-65390"))

test4 = TestCase (assertEqual "readDecInt can read a negative integer with underscores."
                              (-86439)
                              (readDecInt "-8__6___4_3___9"))

-----------------------------------------------------------------------------------------
-- toConstInt

test5 = TestCase (assertEqual "Can parse a literal decimal integer expression."
                              (Left 24369 :: Either Int ExprErr)
                              (toConstInt (DecInt "2_4_3__6_9")))

expr1 = Plus (Times (DecInt "5") (DecInt "2"))
             (Times (Brack (Minus (DecInt "5") (DecInt "2"))) (DecInt "2"))

test6 = TestCase (assertEqual "Can parse a a valid literal integer expression (1/3)."
                              (Left 16 :: Either Int ExprErr)
                              (toConstInt expr1))

test7 = TestCase (assertEqual "Can parse a a valid literal integer expression (2/3)."
                              (Left (-16) :: Either Int ExprErr)
                              (toConstInt (Negate expr1)))

test8 = TestCase (assertEqual "Can parse a a valid literal integer expression (3/3)."
                              (Left 3 :: Either Int ExprErr)
                              (toConstInt (Div expr1 (DecInt "5"))))

expr2 = Plus (Times (DecInt "5") (DecInt "2"))
             (Times (Brack (Minus (DecInt "5") (QasmId "var"))) (DecInt "2"))

test9 = TestCase (assertEqual "toConstInt rejects QasmId expressions."
                              (Right (NonConstId "var") :: Either Int ExprErr)
                              (toConstInt expr2))

expr3 = Plus (Times (DecInt "5") (DecInt "2"))
             (Times (Brack (Minus (DecInt "5") Pi)) (DecInt "2"))

test10 = TestCase (assertEqual "toConstInt rejects Pi expressions."
                               (Right (BadType "angle") :: Either Int ExprErr)
                               (toConstInt expr3))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "readDecInt_Postive_NoUnderscores" test1,
                                     TestLabel "readDecInt_Postive_Underscores" test2,
                                     TestLabel "readDecInt_Negative_NoUnderscores" test3,
                                     TestLabel "readDecInt_Negative_Underscores" test4,
                                     TestLabel "toConstInt_Test1" test5,
                                     TestLabel "toConstInt_Test2" test6,
                                     TestLabel "toConstInt_Test3" test7,
                                     TestLabel "toConstInt_Test4" test8,
                                     TestLabel "toConstInt_Test5" test9,
                                     TestLabel "toConstInt_Test6" test10]

main = defaultMain tests
