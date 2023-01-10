module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified LinguaQuanta.Qasm.Gate as Qasm
import LinguaQuanta.QasmToQuip.Control
import qualified LinguaQuanta.Quip.Gate as Quip

-----------------------------------------------------------------------------------------
-- extractCtrls: no controls

mod0 = Qasm.negateMod $ Qasm.nullGateMod

test1 = TestCase (assertEqual "extractCtrls with zero controls (1/3)."
                              ([], [])
                              (extractCtrls [] mod0))

test2 = TestCase (assertEqual "extractCtrls with zero controls (2/3)."
                              ([1, 2, 3], [])
                              (extractCtrls [1, 2, 3] mod0))

test3 = TestCase (assertEqual "extractCtrls with zero controls (3/3)."
                              ([1, 2, 3, 4, 5, 6], [])
                              (extractCtrls [1, 2, 3, 4, 5, 6] mod0))

-----------------------------------------------------------------------------------------
-- extractCtrls: one control

mod1 = Qasm.negateMod $ Qasm.addCtrlsToMod 1 mod0

test4 = TestCase (assertEqual "extractCtrls with one control (1/3)."
                              ([], [Quip.Pos 1])
                              (extractCtrls [1] mod1))

test5 = TestCase (assertEqual "extractCtrls with one control (2/3)."
                              ([3, 4], [Quip.Pos 2])
                              (extractCtrls [2, 3, 4] mod1))

test6 = TestCase (assertEqual "extractCtrls with one control (3/3)."
                              ([2, 3, 4, 5, 6], [Quip.Pos 1])
                              (extractCtrls [1, 2, 3, 4, 5, 6] mod1))

-----------------------------------------------------------------------------------------
-- extractCtrls: two controls

mod2 = Qasm.addNegCtrlsToMod 1 mod1

test7 = TestCase (assertEqual "extractCtrls with two controls (1/3)."
                              ([], [Quip.Neg 1, Quip.Pos 2])
                              (extractCtrls [1, 2] mod2))

test8 = TestCase (assertEqual "extractCtrls with two controls (2/3)."
                              ([4], [Quip.Neg 2, Quip.Pos 3])
                              (extractCtrls [2, 3, 4] mod2))

test9 = TestCase (assertEqual "extractCtrls with two controls (3/3)."
                              ([3, 4, 5, 6], [Quip.Neg 1, Quip.Pos 2])
                              (extractCtrls [1, 2, 3, 4, 5, 6] mod2))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "esxtractCtrls_0Ctrls_1" test1,
                                     TestLabel "esxtractCtrls_0Ctrls_2" test2,
                                     TestLabel "esxtractCtrls_0Ctrls_3" test3,
                                     TestLabel "esxtractCtrls_1Ctrls_1" test4,
                                     TestLabel "esxtractCtrls_1Ctrls_2" test5,
                                     TestLabel "esxtractCtrls_1Ctrls_3" test6,
                                     TestLabel "esxtractCtrls_2Ctrls_1" test7,
                                     TestLabel "esxtractCtrls_2Ctrls_2" test8,
                                     TestLabel "esxtractCtrls_2Ctrls_3" test9]

main = defaultMain tests
