module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.Expression
import LinguaQuanta.Qasm.Language

-----------------------------------------------------------------------------------------
-- Useful Constructions

-- | Evaluates to 16 as a constant integer.
expr1 = Plus (Times (DecInt "5") (DecInt "2"))
             (Times (Brack (Minus (DecInt "5") (DecInt "2"))) (DecInt "2"))

-- | Evaluates to 3 as a constant integer.
expr2 = Div expr1 (DecInt "5")

-- | Contains a QasmId.
expr3 = Plus (Times (DecInt "5") (DecInt "2"))
             (Times (Brack (Minus (DecInt "5") (QasmId "var"))) (DecInt "2"))

--- | Contains Pi.
expr4 = Plus (Times (DecInt "5") (DecInt "2"))
             (Times (Brack (Minus (DecInt "5") Pi)) (DecInt "2"))

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

test6 = TestCase (assertEqual "Can parse a a valid literal integer expression (1/3)."
                              (Left 16 :: Either Int ExprErr)
                              (toConstInt expr1))

test7 = TestCase (assertEqual "Can parse a a valid literal integer expression (2/3)."
                              (Left (-16) :: Either Int ExprErr)
                              (toConstInt (Negate expr1)))

test8 = TestCase (assertEqual "Can parse a a valid literal integer expression (3/3)."
                              (Left 3 :: Either Int ExprErr)
                              (toConstInt expr2))

test9 = TestCase (assertEqual "toConstInt rejects QasmId expressions."
                              (Right (NonConstId "var") :: Either Int ExprErr)
                              (toConstInt expr3))

test10 = TestCase (assertEqual "toConstInt rejects Pi expressions."
                               (Right (BadType "angle") :: Either Int ExprErr)
                               (toConstInt expr4))

-----------------------------------------------------------------------------------------
-- negate

test11 = TestCase (assertEqual "negateExpr is a syntatctically correct negation (1/4)."
                               (Negate (Brack expr1))
                               (negateExpr expr1))

test12 = TestCase (assertEqual "negateExpr is a syntatctically correct negation (2/4)."
                               (Negate (Brack expr2))
                               (negateExpr expr2))

test13 = TestCase (assertEqual "negateExpr is a syntatctically correct negation (3/4)."
                               (Negate (Brack expr3))
                               (negateExpr expr3))

test14 = TestCase (assertEqual "negateExpr is a syntatctically correct negation (4/4)."
                               (Negate (Brack expr4))
                               (negateExpr expr4))

test15 = TestCase (assertEqual "negateExpr is a semantically correct negation (1/2)."
                               (Left (-16) :: Either Int ExprErr)
                               (toConstInt $ negateExpr expr1))

test16 = TestCase (assertEqual "negateExpr is a semantically correct negation (2/2)."
                               (Left (-3) :: Either Int ExprErr)
                               (toConstInt $ negateExpr expr2))

-----------------------------------------------------------------------------------------
-- zero

test17 = TestCase (assertEqual "Zero expression has correct semantic value."
                               (Left 0 :: Either Int ExprErr)
                               (toConstInt zero))

-----------------------------------------------------------------------------------------
-- avgExpr

test18 = TestCase (assertEqual "avgExpr is a syntatctically correct average (1/4)."
                               (Div (Brack (Plus expr1 expr2)) (DecInt "2"))
                               (avgExpr expr1 expr2))

test19 = TestCase (assertEqual "avgExpr is a syntatctically correct average (2/4)."
                               (Div (Brack (Plus expr1 expr3)) (DecInt "2"))
                               (avgExpr expr1 expr3))

test20 = TestCase (assertEqual "avgExpr is a syntatctically correct average (3/4)."
                               (Div (Brack (Plus expr1 expr4)) (DecInt "2"))
                               (avgExpr expr1 expr4))

test21 = TestCase (assertEqual "avgExpr is a syntatctically correct average (4/4)."
                               (Div (Brack (Plus expr2 expr1)) (DecInt "2"))
                               (avgExpr expr2 expr1))

test22 = TestCase (assertEqual "avgExpr is a semantically correct average (1/2)."
                               (Left 16 :: Either Int ExprErr)
                               (toConstInt $ avgExpr expr1 expr1))

test23 = TestCase (assertEqual "avgExpr is a semantically correct average (2/2)."
                               (Left 9 :: Either Int ExprErr)
                               (toConstInt $ avgExpr expr1 expr2))

-----------------------------------------------------------------------------------------
-- Floats are not integers.

--- | Contains a float.
expr5 = Plus (Times (DecInt "5") (DecInt "2"))
             (Times (Brack (Minus (DecInt "5") (DecFloat "43e-2"))) (DecInt "2"))

test24 = TestCase (assertEqual "toConstInt rejects float expressions."
                               (Right (BadType "float") :: Either Int ExprErr)
                               (toConstInt expr5))

-----------------------------------------------------------------------------------------
-- readFloat

test25 = TestCase (assertEqual "readFloat supports the INT EXP format."
                               13e15
                               (readFloat "1__3e1_5"))

test26 = TestCase (assertEqual "readFloat supports the .INT format."
                               0.435
                               (readFloat ".4__3_5"))

test27 = TestCase (assertEqual "readFloat supports the .INT EXP format."
                               0.43e-2
                               (readFloat ".4__3e-2"))

test28 = TestCase (assertEqual "readFloat supports the INT. format."
                               545.0
                               (readFloat "5_4__5."))

test29 = TestCase (assertEqual "readFloat supports the INT. EXP format."
                               512.0e-3
                               (readFloat "5__1_2.e-3"))

test30 = TestCase (assertEqual "readFloat supports the INT.INT format."
                               435.23
                               (readFloat "4__3___5.2_3"))

test31 = TestCase (assertEqual "readFloat supports the INT.INT EXP format."
                               435.23e14
                               (readFloat "4_3_5.2_3e1_4"))

-----------------------------------------------------------------------------------------
-- toConstFloat

float1 = DecFloat ".5"
float2 = DecInt "5"
float3 = Pi
float4 = Plus float1 float3
float5 = Negate float2
float6 = Brack float5
float7 = Minus float4 float6
float8 = Times float7 $ DecInt "2"
float9 = Div (Plus Pi (Times float2 Pi)) Pi

test32 = TestCase (assertEqual "toConstFloat: floating point literal."
                               (Left 0.5 :: Either Double ExprErr)
                               (toConstFloat float1))

test33 = TestCase (assertEqual "toConstFloat: decimal integer literals."
                               (Left 5.0 :: Either Double ExprErr)
                               (toConstFloat float2))

test34 = TestCase (assertBool "toConstFloat: handling pi."
                              ((abs (3.14 - v)) < 0.01))
    where Left v = toConstFloat float3

test35 = TestCase (assertBool "toConstFloat: handling addition."
                              ((abs (3.64 - v)) < 0.01))
    where Left v = toConstFloat float4

test36 = TestCase (assertEqual "toConstFloat: negation."
                               (Left (-5.0) :: Either Double ExprErr)
                               (toConstFloat float5))

test37 = TestCase (assertEqual "toConstFloat: brackets."
                               (Left (-5.0) :: Either Double ExprErr)
                               (toConstFloat float6))

test38 = TestCase (assertBool "toConstFloat: handling subtraction."
                              ((abs (8.64 - v)) < 0.01))
    where Left v = toConstFloat float7

test39 = TestCase (assertBool "toConstFloat: handling multiplication."
                              ((abs (17.28 - v)) < 0.01))
    where Left v = toConstFloat float8

test40 = TestCase (assertEqual "toConstFloat: handling division."
                               (Left 6.0 :: Either Double ExprErr)
                               (toConstFloat float9))

floaterr = Plus (Times float9 (QasmId "var")) float8

test41 = TestCase (assertEqual "toConstFloat: error detection."
                               (Right (NonConstId "var") :: Either Double ExprErr)
                               (toConstFloat floaterr))

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
                                     TestLabel "toConstInt_Test6" test10,
                                     TestLabel "toConstInt_Test7" test24,
                                     TestLabel "negateExpr_Syntactic_Test1" test11,
                                     TestLabel "negateExpr_Syntactic_Test2" test12,
                                     TestLabel "negateExpr_Syntactic_Test3" test13,
                                     TestLabel "negateExpr_Syntactic_Test4" test14,
                                     TestLabel "negateExpr_Semantic_Test1" test15,
                                     TestLabel "negateExpr_Semantic_Test2" test16,
                                     TestLabel "zero" test17,
                                     TestLabel "avgExpr_Syntactic_Test1" test18,
                                     TestLabel "avgExpr_Syntactic_Test2" test19,
                                     TestLabel "avgExpr_Syntactic_Test3" test20,
                                     TestLabel "avgExpr_Syntactic_Test4" test21,
                                     TestLabel "avgExpr_Semantic_Test1" test22,
                                     TestLabel "avgExpr_Semantic_Test2" test23,
                                     TestLabel "readFloat_Test1" test25,
                                     TestLabel "readFloat_Test2" test26,
                                     TestLabel "readFloat_Test3" test27,
                                     TestLabel "readFloat_Test4" test28,
                                     TestLabel "readFloat_Test5" test29,
                                     TestLabel "readFloat_Test6" test30,
                                     TestLabel "readFloat_Test7" test31,
                                     TestLabel "toConstFloat_Test1" test32,
                                     TestLabel "toConstFloat_Test2" test33,
                                     TestLabel "toConstFloat_Test3" test34,
                                     TestLabel "toConstFloat_Test4" test35,
                                     TestLabel "toConstFloat_Test5" test36,
                                     TestLabel "toConstFloat_Test6" test37,
                                     TestLabel "toConstFloat_Test7" test38,
                                     TestLabel "toConstFloat_Test8" test39,
                                     TestLabel "toConstFloat_Test9" test40,
                                     TestLabel "toConstFloat_Test10" test41]

main = defaultMain tests
