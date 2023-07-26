module Main where

import qualified Data.IntMap.Strict as IntMap
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.Operand
import LinguaQuanta.QasmToQuip.Call
import LinguaQuanta.QasmToQuip.Wire
import LinguaQuanta.Quip.Gate
import LinguaQuanta.Quip.GateName
import LinguaQuanta.Quip.Wire

-----------------------------------------------------------------------------------------
-- Sets up a simple wire allocation map.

(Just allocsTmp0) = allocate QWire "var1" Nothing initialAllocations
(Just allocsTmp1) = allocate QWire "var2" Nothing allocsTmp0
(Just allocsTmp2) = allocate QWire "var3" (Just 3) allocsTmp1
(Just allocs)     = allocate QWire "var4" (Just 5) allocsTmp2

-----------------------------------------------------------------------------------------
-- translateCDiscard

(discardMap1, discardGates1) = translateQDiscard allocs $ QRef "var1"
(discardMap2, discardGates2) = translateQDiscard allocs $ Cell "var3" 2

test1 = TestCase (assertEqual "translateQDiscard: with QRef operand (map)."
                              9
                              (IntMap.size $ toQuipperOutputs discardMap1))

test2 = TestCase (assertEqual "translateQDiscard: with QRef operand (gates)."
                              [QDiscardGate 0]
                              discardGates1)

test3 = TestCase (assertEqual "translateQDiscard: with Cell operand (map)."
                              9
                              (IntMap.size $ toQuipperOutputs discardMap2))

test4 = TestCase (assertEqual "translateQDiscard: with Cell operand (gates)."
                              [QDiscardGate 4]
                              discardGates2)

-----------------------------------------------------------------------------------------
-- translateCInit0

(initZeroMap1, initZeroGates1) = translateQInit0 allocs $ QRef "var1"
(initZeroMap2, initZeroGates2) = translateQInit0 allocs $ Cell "var3" 2

test5 = TestCase (assertEqual "translateQInit0: with QRef operand (map)."
                              9
                              (IntMap.size $ toQuipperInputs initZeroMap1))

test6 = TestCase (assertEqual "translateQInit0: with QRef operand (gates)."
                              [QInitGate False 0]
                              initZeroGates1)

test7 = TestCase (assertEqual "translateQInit0: with Cell operand (map)."
                              9
                              (IntMap.size $ toQuipperInputs initZeroMap2))

test8 = TestCase (assertEqual "translateQInit0: with Cell operand (gates)."
                              [QInitGate False 4]
                              initZeroGates2)

-----------------------------------------------------------------------------------------
-- translateCInit1

(initOneMap1, initOneGates1) = translateQInit1 allocs $ QRef "var1"
(initOneMap2, initOneGates2) = translateQInit1 allocs $ Cell "var3" 2

test9 = TestCase (assertEqual "translateQInit1: with QRef operand (map)."
                              9
                              (IntMap.size $ toQuipperInputs initOneMap1))

test10 = TestCase (assertEqual "translateQInit1: with QRef operand (gates)."
                               [QInitGate True 0]
                               initOneGates1)

test11 = TestCase (assertEqual "translateCQnit1: with Cell operand (map)."
                               9
                               (IntMap.size $ toQuipperInputs initOneMap2))

test12 = TestCase (assertEqual "translateCInit1: with Cell operand (gates)."
                               [QInitGate True 4]
                               initOneGates2)

-----------------------------------------------------------------------------------------
-- translateCTerm0

(termZeroMap1, termZeroGates1) = translateQTerm0 allocs $ QRef "var1"
(termZeroMap2, termZeroGates2) = translateQTerm0 allocs $ Cell "var3" 2

test13 = TestCase (assertEqual "translateQTerm0: with QRef operand (map)."
                               9
                               (IntMap.size $ toQuipperOutputs termZeroMap1))

test14 = TestCase (assertEqual "translateQTerm0: with QRef operand (gates)."
                               [QTermGate False 0]
                               termZeroGates1)

test15 = TestCase (assertEqual "translateQTerm0: with Cell operand (map)."
                               9
                               (IntMap.size $ toQuipperOutputs termZeroMap2))

test16 = TestCase (assertEqual "translateQTerm0: with Cell operand (gates)."
                               [QTermGate False 4]
                               termZeroGates2)

-----------------------------------------------------------------------------------------
-- translateCTerm1

(termOneMap1, termOneGates1) = translateQTerm1 allocs $ QRef "var1"
(termOneMap2, termOneGates2) = translateQTerm1 allocs $ Cell "var3" 2

test17 = TestCase (assertEqual "translateQTerm1: with QRef operand (map)."
                               9
                               (IntMap.size $ toQuipperOutputs termOneMap1))

test18 = TestCase (assertEqual "translateQTerm1: with QRef operand (gates)."
                               [QTermGate True 0]
                               termOneGates1)

test19 = TestCase (assertEqual "translateQTerm1: with Cell operand (map)."
                               9
                               (IntMap.size $ toQuipperOutputs termOneMap2))

test20 = TestCase (assertEqual "translateQTerm1: with Cell operand (gates)."
                               [QTermGate True 4]
                               termOneGates2)

-----------------------------------------------------------------------------------------
-- translateReset

(resetMap1, resetGates1) = translateReset allocs $ QRef "var1"
(resetMap2, resetGates2) = translateReset allocs $ Cell "var3" 2

test21 = TestCase (assertEqual "translateReset: with QRef operand (map)."
                               allocs
                               resetMap1)

test22 = TestCase (assertEqual "translateReset: with QRef operand (gates)."
                               [QDiscardGate 0, QInitGate False 0]
                               resetGates1)


test23 = TestCase (assertEqual "translateReset: with Cell operand (map)."
                               allocs
                               resetMap2)

test24 = TestCase (assertEqual "translateReset: with Cell operand (gates)."
                               [QDiscardGate 4, QInitGate False 4]
                               resetGates2)

-----------------------------------------------------------------------------------------
-- translateVoidMeasure

(measMap1, measGates1) = translateVoidMeasure allocs $ QRef "var1"
(measMap2, measGates2) = translateVoidMeasure measMap1 $ Cell "var3" 2

test25 = TestCase (assertEqual "translateVoidMeasure: with QRef operand (map:size)."
                               (toSize allocs + 1)
                               (toSize measMap1))

test26 = TestCase (assertBool "translateVoidMeasure: with QRef operand (map:loans)."
                              (not $ hasLoans measMap1))

test27 = TestCase (assertEqual "translateVoidMeasure: with Cell operand (map:size)."
                               (toSize allocs + 1)
                               (toSize measMap2))

test28 = TestCase (assertBool "translateVoidMeasure: with Cell operand (map:loans)."
                              (not $ hasLoans measMap1))

test29 = TestCase (assertEqual "translateVoidMeasure: with QRef operand (gates)."
                               [QInitGate False 10,
                                NamedGate GateX False [10] [Pos 0],
                                QMeasGate 10,
                                CDiscardGate 10]
                               measGates1)

test30 = TestCase (assertEqual "translateVoidMeasure: with Cell operand (gates)."
                               [QInitGate False 11,
                                NamedGate GateX False [11] [Pos 4],
                                QMeasGate 11,
                                CDiscardGate 11]
                               measGates2)

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "translateQDiscard_QRef_Map" test1,
                                     TestLabel "translateQDiscard_QRef_Gates" test2,
                                     TestLabel "translateQDiscard_Cell_Map" test3,
                                     TestLabel "translateQDiscard_Cell_Gates" test4,
                                     TestLabel "translateQInit0_QRef_Map" test5,
                                     TestLabel "translateQInit0_QRef_Gates" test6,
                                     TestLabel "translateQInit0_Cell_Map" test7,
                                     TestLabel "translateQInit0_Cell_Gates" test8,
                                     TestLabel "translateQInit1_QRef_Map" test9,
                                     TestLabel "translateQInit1_QRef_Gates" test10,
                                     TestLabel "translateQInit1_Cell_Map" test11,
                                     TestLabel "translateQInit1_Cell_Gates" test12,
                                     TestLabel "translateQTerm0_QRef_Map" test13,
                                     TestLabel "translateQTerm0_QRef_Gates" test14,
                                     TestLabel "translateQTerm0_Cell_Map" test15,
                                     TestLabel "translateQTerm0_Cell_Gates" test16,
                                     TestLabel "translateQTerm1_QRef_Map" test17,
                                     TestLabel "translateQTerm1_QRef_Gates" test18,
                                     TestLabel "translateQTerm1_Cell_Map" test19,
                                     TestLabel "translateQTerm1_Cell_Gates" test20,
                                     TestLabel "translateReset_QRef_Map" test21,
                                     TestLabel "translateReset_QRef_Gates" test22,
                                     TestLabel "translateReset_Cell_Map" test23,
                                     TestLabel "translateReset_Cell_Gates" test24,
                                     TestLabel "translateVoidMeasure_QRef_Size" test25,
                                     TestLabel "translateVoidMeasure_QRef_Loan" test26,
                                     TestLabel "translateVoidMeasure_Cell_Size" test27,
                                     TestLabel "translateVoidMeasure_Cell_Loan" test28,
                                     TestLabel "translateVoidMeasure_QRef_Gates" test29,
                                     TestLabel "translateVoidMeasure_Cell_Gates" test30]

main = defaultMain tests
