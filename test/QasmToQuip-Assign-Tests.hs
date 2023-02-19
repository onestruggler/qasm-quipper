module Main where

import qualified Data.IntMap.Strict as IntMap
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.QasmToQuip.Assign
import LinguaQuanta.QasmToQuip.Wire
import LinguaQuanta.Quip.Gate
import LinguaQuanta.Quip.Wire

-----------------------------------------------------------------------------------------
-- Sets up a simple wire allocation map.

(Just allocsTmp0) = allocate QWire "var1" Nothing initialAllocations
(Just allocsTmp1) = allocate CWire "var2" Nothing allocsTmp0
(Just allocsTmp2) = allocate QWire "var3" (Just 3) allocsTmp1
(Just allocs)     = allocate QWire "var4" (Just 5) allocsTmp2

-----------------------------------------------------------------------------------------
-- translateCDiscard

(discardMap1, discardGates1) = translateCDiscard allocs "var1" Nothing
(discardMap2, discardGates2) = translateCDiscard allocs "var3" $ Just 2

test1 = TestCase (assertEqual "translateCDiscard: with QRef operand (map)."
                              9
                              (IntMap.size $ toQuipperOutputs discardMap1))

test2 = TestCase (assertEqual "translateCDiscard: with QRef operand (gates)."
                              [CDiscardGate 0]
                              discardGates1)

test3 = TestCase (assertEqual "translateCDiscard: with Cell operand (map)."
                              9
                              (IntMap.size $ toQuipperOutputs discardMap2))

test4 = TestCase (assertEqual "translateCDiscard: with Cell operand (gates)."
                              [CDiscardGate 4]
                              discardGates2)

-----------------------------------------------------------------------------------------
-- translateCInit0

(initZeroMap1, initZeroGates1) = translateCInit0 allocs "var1" Nothing
(initZeroMap2, initZeroGates2) = translateCInit0 allocs "var3" $ Just 2

test5 = TestCase (assertEqual "translateCInit0: with QRef operand (map)."
                              9
                              (IntMap.size $ toQuipperInputs initZeroMap1))

test6 = TestCase (assertEqual "translateCInit0: with QRef operand (gates)."
                              [CInitGate False 0]
                              initZeroGates1)

test7 = TestCase (assertEqual "translateCInit0: with Cell operand (map)."
                              9
                              (IntMap.size $ toQuipperInputs initZeroMap2))

test8 = TestCase (assertEqual "translateCInit0: with Cell operand (gates)."
                              [CInitGate False 4]
                              initZeroGates2)

-----------------------------------------------------------------------------------------
-- translateCInit1

(initOneMap1, initOneGates1) = translateCInit1 allocs "var1" Nothing
(initOneMap2, initOneGates2) = translateCInit1 allocs "var3" $ Just 2

test9 = TestCase (assertEqual "translateCInit1: with QRef operand (map)."
                              9
                              (IntMap.size $ toQuipperInputs initOneMap1))

test10 = TestCase (assertEqual "translateCInit1: with QRef operand (gates)."
                               [CInitGate True 0]
                               initOneGates1)

test11 = TestCase (assertEqual "translateCInit1: with Cell operand (map)."
                               9
                               (IntMap.size $ toQuipperInputs initOneMap2))

test12 = TestCase (assertEqual "translateCInit1: with Cell operand (gates)."
                               [CInitGate True 4]
                               initOneGates2)

-----------------------------------------------------------------------------------------
-- translateCTerm0

(termZeroMap1, termZeroGates1) = translateCTerm0 allocs "var1" Nothing
(termZeroMap2, termZeroGates2) = translateCTerm0 allocs "var3" $ Just 2

test13 = TestCase (assertEqual "translateCTerm0: with QRef operand (map)."
                               9
                               (IntMap.size $ toQuipperOutputs termZeroMap1))

test14 = TestCase (assertEqual "translateCTerm0: with QRef operand (gates)."
                               [CTermGate False 0]
                               termZeroGates1)

test15 = TestCase (assertEqual "translateCTerm0: with Cell operand (map)."
                               9
                               (IntMap.size $ toQuipperOutputs termZeroMap2))

test16 = TestCase (assertEqual "translateCTerm0: with Cell operand (gates)."
                               [CTermGate False 4]
                               termZeroGates2)

-----------------------------------------------------------------------------------------
-- translateCTerm1

(termOneMap1, termOneGates1) = translateCTerm1 allocs "var1" Nothing
(termOneMap2, termOneGates2) = translateCTerm1 allocs "var3" $ Just 2

test17 = TestCase (assertEqual "translateCTerm1: with QRef operand (map)."
                               9
                               (IntMap.size $ toQuipperOutputs termOneMap1))

test18 = TestCase (assertEqual "translateCTerm1: with QRef operand (gates)."
                               [CTermGate True 0]
                               termOneGates1)

test19 = TestCase (assertEqual "translateCTerm1: with Cell operand (map)."
                               9
                               (IntMap.size $ toQuipperOutputs termOneMap2))

test20 = TestCase (assertEqual "translateCTerm1: with Cell operand (gates)."
                               [CTermGate True 4]
                               termOneGates2)

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "translateCDiscard_QRef_Map" test1,
                                     TestLabel "translateCDiscard_QRef_Gates" test2,
                                     TestLabel "translateCDiscard_Cell_Map" test3,
                                     TestLabel "translateCDiscard_Cell_Gates" test4,
                                     TestLabel "translateCInit0_QRef_Map" test5,
                                     TestLabel "translateCInit0_QRef_Gates" test6,
                                     TestLabel "translateCInit0_Cell_Map" test7,
                                     TestLabel "translateCInit0_Cell_Gates" test8,
                                     TestLabel "translateCInit1_QRef_Map" test9,
                                     TestLabel "translateCInit1_QRef_Gates" test10,
                                     TestLabel "translateCInit1_Cell_Map" test11,
                                     TestLabel "translateCInit1_Cell_Gates" test12,
                                     TestLabel "translateCTerm0_QRef_Map" test13,
                                     TestLabel "translateCTerm0_QRef_Gates" test14,
                                     TestLabel "translateCTerm0_Cell_Map" test15,
                                     TestLabel "translateCTerm0_Cell_Gates" test16,
                                     TestLabel "translateCTerm1_QRef_Map" test17,
                                     TestLabel "translateCTerm1_QRef_Gates" test18,
                                     TestLabel "translateCTerm1_Cell_Map" test19,
                                     TestLabel "translateCTerm1_Cell_Gates" test20]

main = defaultMain tests