module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.Expression
import LinguaQuanta.Qasm.Gate
import LinguaQuanta.Qasm.GateName
import LinguaQuanta.Qasm.Language

-----------------------------------------------------------------------------------------
-- Gate Modifiers

mod0 = nullGateMod
mod1 = negateMod mod0
mod2 = addCtrlsToMod 2 mod1
mod3 = addNegCtrlsToMod 3 mod2
mod4 = negateMod mod3
mod5 = addCtrlsToMod 1 mod4

test1 = TestCase (assertEqual "Building gate modifiers (1/6)."
                              (GateMod False [])
                              mod0)

test2 = TestCase (assertEqual "Building gate modifiers (2/6)."
                              (GateMod True [])
                              mod1)

test3 = TestCase (assertEqual "Building gate modifiers (3/6)."
                              (GateMod True [Pos, Pos])
                              mod2)

test4 = TestCase (assertEqual "Building gate modifiers (4/6)."
                              (GateMod True [Neg, Neg, Neg, Pos, Pos])
                              mod3)

test5 = TestCase (assertEqual "Building gate modifiers (5/6)."
                              (GateMod False [Neg, Neg, Neg, Pos, Pos])
                              mod4)

test6 = TestCase (assertEqual "Building gate modifiers (6/6)."
                              (GateMod False [Pos, Neg, Neg, Neg, Pos, Pos])
                              mod5)

-----------------------------------------------------------------------------------------
-- Gate Construction

pidiv2    = Div Pi (DecInt "2")
params    = [Pi, pidiv2]
inputs    = [Scalar "q"]
inputsRaw = [QVar "q"]

gate0A = NamedGate GateX params inputs nullGateMod
gate1A = invert gate0A
gate2A = addCtrls 2 gate1A
gate3A = addNegCtrls 3 gate2A
gate4A = invert gate3A
gate5A = addCtrls 1 gate4A

test7 = TestCase (assertEqual "Building named gates (1/6)."
                              (NamedGate GateX params inputs mod0)
                              gate0A)

test8 = TestCase (assertEqual "Building named gates (2/6)."
                              (NamedGate GateX params inputs mod1)
                              gate1A)

test9 = TestCase (assertEqual "Building named gates (3/6)."
                              (NamedGate GateX params inputs mod2)
                              gate2A)

test10 = TestCase (assertEqual "Building named gates (4/6)."
                               (NamedGate GateX params inputs mod3)
                               gate3A)

test11 = TestCase (assertEqual "Building named gates (5/6)."
                               (NamedGate GateX params inputs mod4)
                               gate4A)

test12 = TestCase (assertEqual "Building named gates (6/6)."
                               (NamedGate GateX params inputs mod5)
                               gate5A)

gate0B = GPhaseGate pidiv2 inputs nullGateMod
gate1B = invert gate0B
gate2B = addCtrls 2 gate1B
gate3B = addNegCtrls 3 gate2B
gate4B = invert gate3B
gate5B = addCtrls 1 gate4B

test13 = TestCase (assertEqual "Building phase gates (1/6)."
                               (GPhaseGate pidiv2 inputs mod0)
                               gate0B)

test14 = TestCase (assertEqual "Building phase gates (2/6)."
                               (GPhaseGate pidiv2 inputs mod1)
                               gate1B)

test15 = TestCase (assertEqual "Building phase gates (3/6)."
                               (GPhaseGate pidiv2 inputs mod2)
                               gate2B)

test16 = TestCase (assertEqual "Building phase gates (4/6)."
                               (GPhaseGate pidiv2 inputs mod3)
                               gate3B)

test17 = TestCase (assertEqual "Building phase gates (5/6)."
                               (GPhaseGate pidiv2 inputs mod4)
                               gate4B)

test18 = TestCase (assertEqual "Building phase gates (6/6)."
                               (GPhaseGate pidiv2 inputs mod5)
                               gate5B)

-----------------------------------------------------------------------------------------
-- Gate Evaluation

targetAndCtrl    = [Scalar "v", Cell "reg" 2]
targetAndCtrlRaw = [QVar "v", QReg "reg" (DecInt "2")]

cexprVal3 = Plus (DecInt "4") (DecInt "-1")
cexprVal2 = Times (DecInt "-1") (Minus (DecInt "1") (DecInt "3"))

gateExpr1 = NamedGateOp "crx" [pidiv2] targetAndCtrlRaw
gateExpr2 = NamedGateOp "mygate" params inputsRaw
gateExpr3 = GPhaseOp pidiv2 [] 
gateExpr4 = NegCtrlMod (Just cexprVal3) (CtrlMod (Just cexprVal2) (InvMod gateExpr1))
gateExpr5 = NegCtrlMod (Just cexprVal3) (CtrlMod (Just cexprVal2) (InvMod gateExpr3))
gateExpr6 = InvMod gateExpr4
gateExpr7 = PowMod cexprVal3 gateExpr6

gateEval1 = NamedGate GateCRX [pidiv2] targetAndCtrl nullGateMod
gateEval2 = NamedGate (UserDefined "mygate") params inputs nullGateMod
gateEval3 = GPhaseGate pidiv2 [] nullGateMod
gateEval4 = NamedGate GateCRX [pidiv2] targetAndCtrl mod3
gateEval5 = GPhaseGate pidiv2 [] mod3
gateEval6 = NamedGate GateCRX [pidiv2] targetAndCtrl mod4

test19 = TestCase (assertEqual "Evaluating gate expressions (1/7)."
                               (Left (0, gateEval1) :: Either (Int, Gate) GateSummaryErr)
                               (exprToGate gateExpr1))

test20 = TestCase (assertEqual "Evaluating gate expressions (2/7)."
                               (Left (0, gateEval2) :: Either (Int, Gate) GateSummaryErr)
                               (exprToGate gateExpr2))

test21 = TestCase (assertEqual "Evaluating gate expressions (3/7)."
                               (Left (0, gateEval3) :: Either (Int, Gate) GateSummaryErr)
                               (exprToGate gateExpr3))

test22 = TestCase (assertEqual "Evaluating gate expressions (4/7)."
                               (Left (0, gateEval4) :: Either (Int, Gate) GateSummaryErr)
                               (exprToGate gateExpr4))

test23 = TestCase (assertEqual "Evaluating gate expressions (5/7)."
                               (Left (0, gateEval5) :: Either (Int, Gate) GateSummaryErr)
                               (exprToGate gateExpr5))

test24 = TestCase (assertEqual "Evaluating gate expressions (6/7)."
                               (Left (0, gateEval6) :: Either (Int, Gate) GateSummaryErr)
                               (exprToGate gateExpr6))

test25 = TestCase (assertEqual "Evaluating gate expressions (7/7)."
                               (Left (3, gateEval6) :: Either (Int, Gate) GateSummaryErr)
                               (exprToGate gateExpr7))

badGate1 = PowMod (DecInt "0") gateExpr1
badGate2 = PowMod (DecInt "-1") gateExpr1
badGate3 = PowMod Pi gateExpr1

err1 = NonPosParam 0 (DecInt "0")
err2 = NonPosParam (-1) (DecInt "-1")
err3 = NonConstParam (BadType "angle") Pi

test26 = TestCase (assertEqual "Catching exceptions in gate evaluations (1/3)."
                               (Right err1 :: Either (Int, Gate) GateSummaryErr)
                               (exprToGate badGate1))

test27 = TestCase (assertEqual "Catching exceptions in gate evaluations (2/3)."
                               (Right err2 :: Either (Int, Gate) GateSummaryErr)
                               (exprToGate badGate2))

test28 = TestCase (assertEqual "Catching exceptions in gate evaluations (3/3)."
                               (Right err3 :: Either (Int, Gate) GateSummaryErr)
                               (exprToGate badGate3))

-----------------------------------------------------------------------------------------
-- Gate Validation

doubleControl = (Scalar "w") : targetAndCtrl

validGate1 = NamedGate GateCRX [pidiv2] targetAndCtrl nullGateMod
validGate2 = NamedGate GateCRX [pidiv2] doubleControl (addNegCtrlsToMod 1 nullGateMod)
validGate3 = GPhaseGate pidiv2 [] nullGateMod
validGate4 = GPhaseGate pidiv2 doubleControl (addNegCtrlsToMod 3 nullGateMod)
validGate5 = NamedGate (UserDefined "mygate") [pidiv2] targetAndCtrl nullGateMod

test29 = TestCase (assertEqual "Gate validation (1/5)."
                               (Nothing :: Maybe GateSummaryErr)
                               (validateGate validGate1))

test30 = TestCase (assertEqual "Gate validation (2/5)."
                               (Nothing :: Maybe GateSummaryErr)
                               (validateGate validGate2))

test31 = TestCase (assertEqual "Gate validation (3/5)."
                               (Nothing :: Maybe GateSummaryErr)
                               (validateGate validGate3))

test32 = TestCase (assertEqual "Gate validation (4/5)."
                               (Nothing :: Maybe GateSummaryErr)
                               (validateGate validGate4))

test33 = TestCase (assertEqual "Gate validation (5/5)."
                               (Nothing :: Maybe GateSummaryErr)
                               (validateGate validGate5))

invalidGate1 = NamedGate GateCRX [pidiv2, pidiv2] targetAndCtrl nullGateMod
invalidGate2 = NamedGate GateCRX [pidiv2] doubleControl nullGateMod
invalidGate3 = NamedGate (UserDefined "mygate") [pidiv2] targetAndCtrl mod2
invalidGate4 = GPhaseGate pidiv2 targetAndCtrl nullGateMod

test34 = TestCase (assertEqual "Invalid gate detection (1/4)."
                               (Just (UnexpectedParamCount 2 1) :: Maybe GateSummaryErr)
                               (validateGate invalidGate1))

test35 = TestCase (assertEqual "Invalid gate detection (2/4)."
                               (Just (UnexpectedOperandCount 3 2) :: Maybe GateSummaryErr)
                               (validateGate invalidGate2))

test36 = TestCase (assertEqual "Invalid gate detection (3/4)."
                               (Just (UnexpectedOperandCount 2 3) :: Maybe GateSummaryErr)
                               (validateGate invalidGate3))

test37 = TestCase (assertEqual "Invalid gate detection (4/4)."
                               (Just (UnexpectedOperandCount 2 0) :: Maybe GateSummaryErr)
                               (validateGate invalidGate4))

-----------------------------------------------------------------------------------------
-- Inversion Detection

test38 = TestCase (assertBool "Inversion modifier detected (1/2)."
                              (not $ hasInversionMod mod0))

test39 = TestCase (assertBool "Inversion modifier detected (2/2)."
                              (hasInversionMod mod1))

test40 = TestCase (assertBool "Inverted gate detected (1/2)."
                              (not $ isInverted validGate1))

test41 = TestCase (assertBool "Inverted gate detected (2/2)."
                              (isInverted gateEval4))

-----------------------------------------------------------------------------------------
-- Operand Conversion

negOperandGate = NamedGateOp "x" [] [QReg "arr" (DecInt "-1")]
nonConstOpGate = NamedGateOp "x" [] [QReg "arr" (QasmId "id")]

negOperandErr = NegArrIndex (-1) (DecInt "-1")
nonConstOpErr = NonConstArrIndex (NonConstId "id") (QasmId "id")

test42 = TestCase (assertEqual "Negative operand cells are rejected."
                               (Right negOperandErr :: Either (Int, Gate) GateSummaryErr)
                               (exprToGate negOperandGate))

test43 = TestCase (assertEqual "Cell operands must have compile-time constant indices."
                               (Right nonConstOpErr :: Either (Int, Gate) GateSummaryErr)
                               (exprToGate nonConstOpGate))

-----------------------------------------------------------------------------------------
-- Control Extraction

test44 = TestCase (assertEqual "getCtrlList (1/6)."
                               []
                               (getCtrlList mod0))

test45 = TestCase (assertEqual "getCtrlList (2/6)."
                               []
                               (getCtrlList mod1))

test46 = TestCase (assertEqual "getCtrlList (3/6)."
                               [Pos, Pos]
                               (getCtrlList mod2))

test47 = TestCase (assertEqual "getCtrlList (4/6)."
                               [Neg, Neg, Neg, Pos, Pos]
                               (getCtrlList mod3))

test48 = TestCase (assertEqual "getCtrlList (5/6)."
                               [Neg, Neg, Neg, Pos, Pos]
                               (getCtrlList mod4))

test49 = TestCase (assertEqual "getCtrlList (6/6)."
                               [Pos, Neg, Neg, Neg, Pos, Pos]
                               (getCtrlList mod5))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "GateMod_1" test1,
                                     TestLabel "GateMod_1" test2,
                                     TestLabel "GateMod_1" test3,
                                     TestLabel "GateMod_1" test4,
                                     TestLabel "GateMod_1" test5,
                                     TestLabel "GateMod_1" test6,
                                     TestLabel "NamedGate_1" test7,
                                     TestLabel "NamedGate_2" test8,
                                     TestLabel "NamedGate_3" test9,
                                     TestLabel "NamedGate_4" test10,
                                     TestLabel "NamedGate_5" test11,
                                     TestLabel "NamedGate_6" test12,
                                     TestLabel "PhaseGate_1" test13,
                                     TestLabel "PhaseGate_2" test14,
                                     TestLabel "PhaseGate_3" test15,
                                     TestLabel "PhaseGate_4" test16,
                                     TestLabel "PhaseGate_5" test17,
                                     TestLabel "PhaseGate_6" test18,
                                     TestLabel "GateEval_1" test19,
                                     TestLabel "GateEval_2" test20,
                                     TestLabel "GateEval_3" test21,
                                     TestLabel "GateEval_4" test22,
                                     TestLabel "GateEval_5" test23,
                                     TestLabel "GateEval_6" test24,
                                     TestLabel "GateEval_7" test25,
                                     TestLabel "BadGate_1" test26,
                                     TestLabel "BadGate_2" test27,
                                     TestLabel "BadGate_3" test28,
                                     TestLabel "Validate_1" test29,
                                     TestLabel "Validate_2" test30,
                                     TestLabel "Validate_3" test31,
                                     TestLabel "Validate_4" test32,
                                     TestLabel "Validate_5" test33,
                                     TestLabel "Invalidated_1" test34,
                                     TestLabel "Invalidated_2" test35,
                                     TestLabel "Invalidated_3" test36,
                                     TestLabel "Invalidated_4" test37,
                                     TestLabel "IsInverted_Mod_1" test38,
                                     TestLabel "IsInverted_Mod_2" test39,
                                     TestLabel "IsInverted_Gate_1" test40,
                                     TestLabel "IsInverted_Gate_2" test41,
                                     TestLabel "OperandNonNeg" test42,
                                     TestLabel "OperandConst" test43,
                                     TestLabel "getCtrlList_1" test44,
                                     TestLabel "getCtrlList_2" test45,
                                     TestLabel "getCtrlList_3" test46,
                                     TestLabel "getCtrlList_4" test47,
                                     TestLabel "getCtrlList_5" test48,
                                     TestLabel "getCtrlList_6" test49]

main = defaultMain tests
