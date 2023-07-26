module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.Expression
import LinguaQuanta.Qasm.Language
import LinguaQuanta.Qasm.Operand

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
-- Constant Integer Calls

call1 = Call "mod" [DecInt "5", DecInt "3"]

call2 = Call "mod" [expr2, expr2]

call3 = Call "pow" [expr1, expr2]

call4 = Call "pow" [expr2, expr2]

call5 = Call "mod" [expr1]

call6 = Call "pow" [expr1, expr1, expr1]

call7 = Call "xyz" [expr1]

call8 = Call "pow" [expr2, Negate expr2]

test42 = TestCase (assertEqual "toConstInt: mod (1/2)."
                              (Left 2 :: Either Int ExprErr)
                              (toConstInt call1))

test43 = TestCase (assertEqual "toConstInt: mod (2/2)."
                              (Left 0 :: Either Int ExprErr)
                              (toConstInt call2))

test44 = TestCase (assertEqual "toConstInt: pow (1/2)."
                              (Left 4096 :: Either Int ExprErr)
                              (toConstInt call3))

test45 = TestCase (assertEqual "toConstInt: pow (2/2)."
                              (Left 27 :: Either Int ExprErr)
                              (toConstInt call4))

test46 = TestCase (assertEqual "toConstInt: mod arity."
                               (Right err :: Either Int ExprErr)
                               (toConstInt call5))
    where err = CallArityMismatch "mod" 1 2

test47 = TestCase (assertEqual "toConstInt: pow arity."
                               (Right err :: Either Int ExprErr)
                               (toConstInt call6))
    where err = CallArityMismatch "pow" 3 2

test48 = TestCase (assertEqual "toConstInt: unknown function."
                               (Right err :: Either Int ExprErr)
                               (toConstInt call7))
    where err = UnknownCall "xyz" 1

test49 = TestCase (assertEqual "toConstInt: unknown function."
                               (Right (NegIntExp (-3)) :: Either Int ExprErr)
                               (toConstInt call8))

-----------------------------------------------------------------------------------------
-- Symbolic Real Calls

call9 = Call "arccos" [DecInt "1"]

call10 = Call "arccos" [DecInt "-1"]

call11 = Call "arcsin" [DecInt "1"]

call12 = Call "arcsin" [DecInt "-1"]

call13 = Call "arctan" [DecInt "0"]

call14 = Call "arctan" [DecInt "1"]

call15 = Call "cos" [Pi]

call16 = Call "cos" [Div Pi $ DecInt "2"]

call17 = Call "sin" [Pi]

call18 = Call "sin" [Div Pi $ DecInt "2"]

call19 = Call "tan" [DecInt "0"]

call20 = Call "tan" [Div Pi $ DecInt "4"]

call21 = Call "exp" [DecInt "0"]

call22 = Call "exp" [DecInt "1"]

call23 = Call "sqrt" [DecInt "4"]

call24 = Call "sqrt" [DecInt "9"]

call25 = Call "log" [DecInt "1"]

call26 = Call "log" [DecFloat "2.71828"]

call27 = Call "pow" [DecInt "4", DecFloat "0.5"]

call28 = Call "pow" [DecInt "2", DecInt "-1"]

call29 = Call "ceiling" [DecFloat "2.7"]

call30 = Call "ceiling" [DecFloat "3.65"]

call31 = Call "floor" [DecFloat "2.7"]

call32 = Call "floor" [DecFloat "3.65"]

call33 = Call "mod" [DecFloat "5.2", DecFloat "3.1"]

call34 = Call "mod" [DecFloat "10.2", DecInt "5"]

call35 = Call "arccos" [DecInt "1", DecInt "2"]

call36 = Call "mod" [DecFloat "5.2"]

call37 = Call "xyz" [DecFloat "5.2"]

test50 = TestCase (assertEqual "toConstFloat: arccos (1/2)."
                               (Left 0 :: Either Double ExprErr)
                               (toConstFloat call9))

test51 = TestCase (assertBool "toConstFloat: arccos (2/2)."
                              ((abs (3.14 - v)) < 0.01))
    where Left v = toConstFloat call10

test52 = TestCase (assertBool "toConstFloat: arcsin (1/2)."
                              ((abs (1.57 - v)) < 0.01))
    where Left v = toConstFloat call11

test53 = TestCase (assertBool "toConstFloat: arcsin (2/2)."
                              ((abs (1.57 + v)) < 0.01))
    where Left v = toConstFloat call12

test54 = TestCase (assertEqual "toConstFloat: arctan (1/2)."
                               (Left 0 :: Either Double ExprErr)
                               (toConstFloat call13))

test55 = TestCase (assertBool "toConstFloat: arctan (2/2)."
                              ((abs (0.78 - v)) < 0.01))
    where Left v = toConstFloat call14

test56 = TestCase (assertEqual "toConstFloat: cos (1/2)."
                               (Left (-1) :: Either Double ExprErr)
                               (toConstFloat call15))

test57 = TestCase (assertBool "toConstFloat: cos (2/2)."
                              (abs v < 0.0001))
    where Left v = toConstFloat call16

test58 = TestCase (assertBool "toConstFloat: sin (1/2)."
                              (abs v < 0.0001))
    where Left v = toConstFloat call17

test59 = TestCase (assertEqual "toConstFloat: sin (2/2)."
                               (Left 1 :: Either Double ExprErr)
                               (toConstFloat call18))

test60 = TestCase (assertEqual "toConstFloat: tan (1/2)."
                               (Left 0 :: Either Double ExprErr)
                               (toConstFloat call19))

test61 = TestCase (assertBool "toConstFloat: tan (2/2)."
                              ((abs (1 - v)) < 0.0001))
    where Left v = toConstFloat call20

test62 = TestCase (assertEqual "toConstFloat: exp (1/2)."
                               (Left 1 :: Either Double ExprErr)
                               (toConstFloat call21))

test63 = TestCase (assertBool "toConstFloat: exp (2/2)."
                              ((abs (2.71 - v)) < 0.01))
    where Left v = toConstFloat call22

test64 = TestCase (assertEqual "toConstFloat: sqrt (1/2)."
                               (Left 2 :: Either Double ExprErr)
                               (toConstFloat call23))

test65 = TestCase (assertEqual "toConstFloat: sqrt (2/2)."
                               (Left 3 :: Either Double ExprErr)
                               (toConstFloat call24))

test66 = TestCase (assertEqual "toConstFloat: log (1/2)."
                               (Left 0 :: Either Double ExprErr)
                               (toConstFloat call25))

test67 = TestCase (assertBool "toConstFloat: log (2/2)."
                              ((abs (1 - v)) < 0.001))
    where Left v = toConstFloat call26

test68 = TestCase (assertEqual "toConstFloat: pow (1/2)."
                               (Left 2.0 :: Either Double ExprErr)
                               (toConstFloat call27))

test69 = TestCase (assertEqual "toConstFloat: pow (2/2)."
                               (Left 0.5 :: Either Double ExprErr)
                               (toConstFloat call28))

test70 = TestCase (assertEqual "toConstFloat: ceiling (1/2)."
                               (Left 3 :: Either Double ExprErr)
                               (toConstFloat call29))

test71 = TestCase (assertEqual "toConstFloat: ceiling (2/2)."
                               (Left 4 :: Either Double ExprErr)
                               (toConstFloat call30))

test72 = TestCase (assertEqual "toConstFloat: floor (1/2)."
                               (Left 2 :: Either Double ExprErr)
                               (toConstFloat call31))

test73 = TestCase (assertEqual "toConstFloat: floor (2/2)."
                               (Left 3 :: Either Double ExprErr)
                               (toConstFloat call32))

test74 = TestCase (assertEqual "toConstFloat: mod (1/2)."
                               (Left 2 :: Either Double ExprErr)
                               (toConstFloat call33))

test75 = TestCase (assertEqual "toConstFloat: mod (2/2)."
                               (Left 0 :: Either Double ExprErr)
                               (toConstFloat call34))

test76 = TestCase (assertEqual "toConstFloat: arccos arity."
                               (Right err :: Either Double ExprErr)
                               (toConstFloat call35))
    where err = CallArityMismatch "arccos" 2 1

test77 = TestCase (assertEqual "toConstFloat: mod arity."
                               (Right err :: Either Double ExprErr)
                               (toConstFloat call36))
    where err = CallArityMismatch "mod" 1 2

test78 = TestCase (assertEqual "toConstFloat: unknown function."
                               (Right err :: Either Double ExprErr)
                               (toConstFloat call37))
    where err = UnknownCall "xyz" 1

-----------------------------------------------------------------------------------------
-- Tau and Euler

test79 = TestCase (assertEqual "toConstInt: Tau."
                               (Right (BadType "angle") :: Either Int ExprErr)
                               (toConstInt Tau))

test80 = TestCase (assertEqual "toConstInt: Euler."
                               (Right (BadType "float") :: Either Int ExprErr)
                               (toConstInt Euler))

test81 = TestCase (assertBool "toConstFloat: Tau."
                              ((abs (6.28 - v)) < 0.01))
    where Left v = toConstFloat Tau

test82 = TestCase (assertBool "toConstFloat: Euler."
                              ((abs (2.71 - v)) < 0.01))
    where Left v = toConstFloat Euler

-----------------------------------------------------------------------------------------
-- Evaluating Non-Constant Cells.

test83 = TestCase (assertEqual "toConstInt rejects QasmCell expressions."
                               (Right (NonConstId "var") :: Either Int ExprErr)
                               (toConstInt $ QasmCell "var" $ DecInt "1"))

test84 = TestCase (assertEqual "toConstFloat rejects QasmCell expressions."
                               (Right (NonConstId "var") :: Either Double ExprErr)
                               (toConstFloat $ QasmCell "var" $ DecInt "1"))

-----------------------------------------------------------------------------------------
-- toRValue

rval1 = Call "QMeas" [QasmId "q"]
rval2 = Call "QMeas" [QasmCell "var" $ DecInt "5"]
rval3 = Call "QMeas" [QasmId "q1", QasmId "Q2"]
rval4 = Call "QMeas" [Pi]

rvalerr1 = CallArityMismatch "QMeas" 2 1

test85 = TestCase (assertEqual "toRValue: QMeas (1/3)."
                               (Left (QuipMeasure $ QRef "q") :: Either RValue ExprErr)
                               (toRValue rval1))

test86 = TestCase (assertEqual "toRValue: QMeas (2/3)."
                               (Left (QuipMeasure $ Cell "var" 5) :: Either RValue ExprErr)
                               (toRValue rval2))

test87 = TestCase (assertEqual "toRValue: QMeas (3/3)."
                               (Left (QuipMeasure $ QRef "q") :: Either RValue ExprErr)
                               (toRValue $ Brack $ Brack $ Brack rval1))

test88 = TestCase (assertEqual "toRValue error, QMeas with too many arguments."
                               (Right rvalerr1 :: Either RValue ExprErr)
                               (toRValue rval3))

test89 = TestCase (assertEqual "toRValue error, QMeas with non-operand expression."
                               (Right NonOperandExpr :: Either RValue ExprErr)
                               (toRValue rval4))

test90 = TestCase (assertEqual "toRValue error, non-rvalue expression."
                               (Right UnknownRValue :: Either RValue ExprErr)
                               (toRValue Pi))

-----------------------------------------------------------------------------------------
-- Ancilla RValues

ancilla_rval1 = Call "CInit0" []
ancilla_rval2 = Call "CInit1" []
ancilla_rval3 = Call "CTerm0" []
ancilla_rval4 = Call "CTerm1" []
ancilla_rval5 = Call "CDiscard" []
ancilla_error = Call "CInit0" [Pi]

test91 = TestCase (assertEqual "toRValue: ancillas (1/5)."
                               (Left QuipCInit0 :: Either RValue ExprErr)
                               (toRValue ancilla_rval1))

test92 = TestCase (assertEqual "toRValue: ancillas (2/5)."
                               (Left QuipCInit1 :: Either RValue ExprErr)
                               (toRValue ancilla_rval2))

test93 = TestCase (assertEqual "toRValue: ancillas (3/5)."
                               (Left QuipCTerm0 :: Either RValue ExprErr)
                               (toRValue ancilla_rval3))

test94 = TestCase (assertEqual "toRValue: ancillas (4/5)."
                               (Left QuipCTerm1 :: Either RValue ExprErr)
                               (toRValue ancilla_rval4))

test95 = TestCase (assertEqual "toRValue: ancillas (5/5)."
                               (Left QuipCDiscard :: Either RValue ExprErr)
                               (toRValue ancilla_rval5))

test96 = TestCase (assertEqual "toRValue rejects ancilla calls of incorrect arity."
                               (Right error :: Either RValue ExprErr)
                               (toRValue ancilla_error))
    where error = CallArityMismatch "CInit0" 1 0

-----------------------------------------------------------------------------------------
-- toVoidCall

test97 = TestCase (assertEqual "toVoidCall: ancillas (1/5)."
                               (Left $ QuipQInit0 $ QRef "q" :: Either VoidCall ExprErr)
                               (toVoidCall "QInit0" [QasmId "q"]))

test98 = TestCase (assertEqual "toVoidCall: ancillas (2/5)."
                               (Left $ QuipQInit1 $ QRef "qb" :: Either VoidCall ExprErr)
                               (toVoidCall "QInit1" [QasmId "qb"]))

test99 = TestCase (assertEqual "toVoidCall: ancillas (3/5)."
                               (Left $ QuipQTerm0 $ Cell "qs" 4 :: Either VoidCall ExprErr)
                               (toVoidCall "QTerm0" [QasmCell "qs" $ DecInt "4"]))

test100 = TestCase (assertEqual "toVoidCall: ancillas (4/5)."
                                (Left $ QuipQTerm1 $ Cell "r" 7 :: Either VoidCall ExprErr)
                                (toVoidCall "QTerm1" [QasmCell "r" $ DecInt "7"]))

test101 = TestCase (assertEqual "toVoidCall: ancillas (5/5)."
                                (Left $ QuipQDiscard $ QRef "x" :: Either VoidCall ExprErr)
                                (toVoidCall "QDiscard" [QasmId "x"]))

test102 = TestCase (assertEqual "toVoidCall rejects unknown calls."
                                (Right $ UnknownCall "blah" 1 :: Either VoidCall ExprErr)
                                (toVoidCall "blah" [QasmId "x"]))

test103 = TestCase (assertEqual "toVoidCall rejects non-operands parameters."
                                (Right NonOperandExpr :: Either VoidCall ExprErr)
                                (toVoidCall "QDiscard" [Pi]))

test104 = TestCase (assertEqual "toVoidCall rejects calls of incorrect arity."
                                (Right error :: Either VoidCall ExprErr)
                                (toVoidCall "QInit0" [QasmId "x", QasmId "y"]))
    where error = CallArityMismatch "QInit0" 2 1

-----------------------------------------------------------------------------------------
-- Support for QasmMeasure expression.

test105 = TestCase (assertEqual "toConstInt rejects QasmMeasure."
                                (Right UnexpectedMeasure :: Either Int ExprErr)
                                (toConstInt $ QasmMeasure $ QVar "x"))

test106 = TestCase (assertEqual "toConstInt rejects QasmMeasure."
                                (Right UnexpectedMeasure :: Either Double ExprErr)
                                (toConstFloat $ QasmMeasure $ QVar "x"))

test107 = TestCase (assertEqual "toRValue supports valid QasmMeasure statements."
                                (Left $ Measure $ QRef "x" :: Either RValue ExprErr)
                                (toRValue $ QasmMeasure $ QVar "x"))

test108 = TestCase (assertEqual "toRValue rejects QasmMeasure statements with bad idxs."
                                (Right $ NegArrIdx (-5) :: Either RValue ExprErr)
                                (toRValue $ QasmMeasure $ QReg "x" $ DecInt "-5"))

-----------------------------------------------------------------------------------------
-- parseGateOperand.

test109 = TestCase (assertEqual "parseGateOperand supports QVar."
                                (Left $ QRef "x" :: Either Operand ExprErr)
                                (parseGateOperand $ QVar "x"))

test110 = TestCase (assertEqual "parseGateOperand supports QReg."
                                (Left $ Cell "x" 5 :: Either Operand ExprErr)
                                (parseGateOperand $ QReg "x" $ DecInt "5"))

test111 = TestCase (assertEqual "parseGateOperand rejects bad indices."
                                (Right $ NegArrIdx (-5) :: Either Operand ExprErr)
                                (parseGateOperand $ QReg "x" $ DecInt "-5"))

-----------------------------------------------------------------------------------------
-- euler.

test112 = TestCase (assertBool "Euler literal is correct."
                               ((abs (2.71 - v)) < 0.01))
    where Left v = toConstFloat euler

-----------------------------------------------------------------------------------------
-- toQasm3.

test113 = TestCase (assertEqual "toQasm3 rewrites all instances of ln(-)."
                                (expr $ Call "log" [Euler])
                                (toQasm3 $ expr $ Call "ln" [Euler]))
    where expr term = Plus (Minus (Negate term) (Brack term))
                           (Div (Times (Call "exp" [Pi, Tau, Euler, term]) term)
                                (QasmId "var"))

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
                                     TestLabel "toConstFloat_Test10" test41,
                                     TestLabel "toConstInt_Call_Test1" test42,
                                     TestLabel "toConstInt_Call_Test2" test43,
                                     TestLabel "toConstInt_Call_Test3" test44,
                                     TestLabel "toConstInt_Call_Test4" test45,
                                     TestLabel "toConstInt_Call_Test5" test46,
                                     TestLabel "toConstInt_Call_Test6" test47,
                                     TestLabel "toConstInt_Call_Test7" test48,
                                     TestLabel "toConstInt_Call_Test8" test49,
                                     TestLabel "toConstFloat_Call_Test1" test50,
                                     TestLabel "toConstFloat_Call_Test2" test51,
                                     TestLabel "toConstFloat_Call_Test3" test52,
                                     TestLabel "toConstFloat_Call_Test4" test53,
                                     TestLabel "toConstFloat_Call_Test5" test54,
                                     TestLabel "toConstFloat_Call_Test6" test55,
                                     TestLabel "toConstFloat_Call_Test7" test56,
                                     TestLabel "toConstFloat_Call_Test8" test57,
                                     TestLabel "toConstFloat_Call_Test9" test58,
                                     TestLabel "toConstFloat_Call_Test10" test59,
                                     TestLabel "toConstFloat_Call_Test11" test60,
                                     TestLabel "toConstFloat_Call_Test12" test61,
                                     TestLabel "toConstFloat_Call_Test13" test62,
                                     TestLabel "toConstFloat_Call_Test14" test63,
                                     TestLabel "toConstFloat_Call_Test15" test64,
                                     TestLabel "toConstFloat_Call_Test16" test65,
                                     TestLabel "toConstFloat_Call_Test17" test66,
                                     TestLabel "toConstFloat_Call_Test18" test67,
                                     TestLabel "toConstFloat_Call_Test19" test68,
                                     TestLabel "toConstFloat_Call_Test20" test69,
                                     TestLabel "toConstFloat_Call_Test21" test70,
                                     TestLabel "toConstFloat_Call_Test22" test71,
                                     TestLabel "toConstFloat_Call_Test23" test72,
                                     TestLabel "toConstFloat_Call_Test24" test73,
                                     TestLabel "toConstFloat_Call_Test25" test74,
                                     TestLabel "toConstFloat_Call_Test26" test75,
                                     TestLabel "toConstFloat_Call_Test27" test76,
                                     TestLabel "toConstFloat_Call_Test28" test77,
                                     TestLabel "toConstFloat_Call_Test29" test78,
                                     TestLabel "toConstInt_Tau" test79,
                                     TestLabel "toConstInt_Euler" test80,
                                     TestLabel "toConstFloat_Tau" test81,
                                     TestLabel "toConstFloat_Euler" test82,
                                     TestLabel "toConstInt_QasmCell" test83,
                                     TestLabel "toConstFloat_QasmCell" test84,
                                     TestLabel "toRValue_Test1" test85,
                                     TestLabel "toRValue_Test2" test86,
                                     TestLabel "toRValue_Test3" test87,
                                     TestLabel "toRValue_Test4" test88,
                                     TestLabel "toRValue_Test5" test89,
                                     TestLabel "toRValue_Test6" test90,
                                     TestLabel "toRValue_Ancilla_Test1" test91,
                                     TestLabel "toRValue_Ancilla_Test2" test92,
                                     TestLabel "toRValue_Ancilla_Test3" test93,
                                     TestLabel "toRValue_Ancilla_Test4" test94,
                                     TestLabel "toRValue_Ancilla_Test5" test95,
                                     TestLabel "toRValue_Ancilla_Error" test96,
                                     TestLabel "toVoidCall_Ancilla_Test1" test97,
                                     TestLabel "toVoidCall_Ancilla_Test2" test98,
                                     TestLabel "toVoidCall_Ancilla_Test3" test99,
                                     TestLabel "toVoidCall_Ancilla_Test4" test100,
                                     TestLabel "toVoidCall_Ancilla_Test5" test101,
                                     TestLabel "toVoidCall_Unknown_Error" test102,
                                     TestLabel "toVoidCall_NonOperand_Error" test103,
                                     TestLabel "toVoidCall_Arity_Error" test104,
                                     TestLabel "toConstInt_QasmMeasure" test105,
                                     TestLabel "toConstFloat_QasmMeasure" test106,
                                     TestLabel "toRValue_QasmMeasure" test107,
                                     TestLabel "toRValue_QasmMeasure_NonOp" test108,
                                     TestLabel "toGateOperand_QVar" test109,
                                     TestLabel "toGateOperand_QReg" test110,
                                     TestLabel "toGateOperand_BadIndex" test111,
                                     TestLabel "euler_literal" test112,
                                     TestLabel "toQasm3" test113]

main = defaultMain tests