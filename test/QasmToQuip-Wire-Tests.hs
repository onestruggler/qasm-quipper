module Main where

import Data.Maybe
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Quip.Wire
import QasmToQuip.Wire

-----------------------------------------------------------------------------------------
-- Tests DFA refinement of the WireState API.

getLeft :: Either a b -> a
getLeft (Left x) = x
getLeft _        = error "Expected left-value."

getRight :: Either a b -> b
getRight (Right x) = x
getRight _         = error "Expected right-value."

-- Checks that there are at least 5 reachable states, accessible as in the DFA.

state0 = allocateWire 5
state1 = getRight $ initWire state0
state2 = getRight $ termWire state0
state3 = getRight $ useWire state0
state4 = getRight $ termWire state1

test1 = TestCase (assertBool "WireState API: Distinct states (1/10)."
                             (state0 /= state1))

test2 = TestCase (assertBool "WireState API: Distinct states (2/10)."
                             (state0 /= state2))

test3 = TestCase (assertBool "WireState API: Distinct states (3/10)."
                             (state0 /= state3))

test4 = TestCase (assertBool "WireState API: Distinct states (4/10)."
                             (state0 /= state4))

test5 = TestCase (assertBool "WireState API: Distinct states (5/10)."
                             (state1 /= state2))

test6 = TestCase (assertBool "WireState API: Distinct states (6/10)."
                             (state1 /= state3))

test7 = TestCase (assertBool "WireState API: Distinct states (7/10)."
                             (state1 /= state4))

test8 = TestCase (assertBool "WireState API: Distinct states (8/10)."
                             (state2 /= state3))

test9 = TestCase (assertBool "WireState API: Distinct states (9/10)."
                             (state2 /= state4))

test10 = TestCase (assertBool "WireState API: Distinct states (10/10)."
                              (state3 /= state4))

-- Checks that invalid actions result in error states.

test11 = TestCase (assertEqual "WireState API: Expected errors (1/6)."
                               (Left DoubleInit)
                               (initWire state1))

test12 = TestCase (assertEqual "WireState API: Expected errors (2/6)."
                               (Left UseBeforeInit)
                               (useWire state2))

test13 = TestCase (assertEqual "WireState API: Expected errors (3/6)."
                               (Left DoubleTerm)
                               (termWire state2))

test14 = TestCase (assertEqual "WireState API: Expected errors (4/6)."
                               (Left DoubleInit)
                               (initWire state3))

test15 = TestCase (assertEqual "WireState API: Expected errors (5/6)."
                               (Left UseBeforeInit)
                               (useWire state4))

test16 = TestCase (assertEqual "WireState API: Expected errors (6/6)."
                               (Left DoubleTerm)
                               (termWire state4))

-- Checks that the five states are closed under non-erroneous transitions.

test17 = TestCase (assertEqual "WireState API: Expected transitions (1/5)."
                               (Right state1)
                               (useWire state1))

test18 = TestCase (assertEqual "WireState API: Expected transitions (2/5)."
                               (Right state1)
                               (initWire state4))

test19 = TestCase (assertEqual "WireState API: Expected transitions (3/5)."
                               (Right state3)
                               (initWire state2))

test20 = TestCase (assertEqual "WireState API: Expected transitions (4/5)."
                               (Right state2)
                               (termWire state3))

test21 = TestCase (assertEqual "WireState API: Expected transitions (5/5)."
                               (Right state3)
                               (useWire state3))

-----------------------------------------------------------------------------------------
-- Tests state properties within WireState API.

state0' = allocateWire 10

test22 = TestCase (assertBool "WireState API: isOutput (1/5)."
                              (isOutput state0))

test23 = TestCase (assertBool "WireState API: isOutput (2/5)."
                              (isOutput state1))

test24 = TestCase (assertBool "WireState API: isOutput (3/5)."
                              (not (isOutput state2)))

test25 = TestCase (assertBool "WireState API: isOutput (4/5)."
                              (isOutput state3))

test26 = TestCase (assertBool "WireState API: isOutput (5/5)."
                              (not (isOutput state4)))

test27 = TestCase (assertBool "WireState API: isInput (1/5)."
                              (isInput state0))

test28 = TestCase (assertBool "WireState API: isInput (2/5)."
                              (not (isInput state1)))

test29 = TestCase (assertBool "WireState API: isInput (3/5)."
                              (isInput state2))

test30 = TestCase (assertBool "WireState API: isInput (4/5)."
                              (isInput state3))

test31 = TestCase (assertBool "WireState API: isInput (5/5)."
                              (not (isInput state4)))

test32 = TestCase (assertEqual "WireState API: wireIndex (1/6)"
                               5
                               (wireIndex state0))

test33 = TestCase (assertEqual "WireState API: wireIndex (2/6)"
                               (wireIndex state0)
                               (wireIndex state1))

test34 = TestCase (assertEqual "WireState API: wireIndex (3/6)"
                               (wireIndex state0)
                               (wireIndex state2))

test35 = TestCase (assertEqual "WireState API: wireIndex (4/6)"
                               (wireIndex state0)
                               (wireIndex state3))

test36 = TestCase (assertEqual "WireState API: wireIndex (5/6)"
                               (wireIndex state0)
                               (wireIndex state4))

test37 = TestCase (assertEqual "WireState API: wireIndex (6/6)."
                               10
                               (wireIndex state0'))

-----------------------------------------------------------------------------------------
-- Wire Allocation Map Initialization.

alloc0 = initialAllocations
alloc1 = fromJust $ allocate QWire "decl1" Nothing  alloc0
alloc2 = fromJust $ allocate CWire "decl2" (Just 5) alloc1
alloc3 = fromJust $ allocate QWire "decl3" (Just 4) alloc2
alloc4 = fromJust $ allocate CWire "decl4" Nothing  alloc3

test38 = TestCase (assertEqual "WireAllocMap: initialization (1/5)."
                               Undeclared
                               (getDeclType "decl1" alloc0))

test39 = TestCase (assertEqual "WireAllocMap: initialization (2/5)."
                               Undeclared
                               (getDeclType "decl2" alloc0))

test40 = TestCase (assertEqual "WireAllocMap: initialization (3/5)."
                               Undeclared
                               (getDeclType "decl3" alloc0))

test41 = TestCase (assertEqual "WireAllocMap: initialization (4/5)."
                               Undeclared
                               (getDeclType "decl4" alloc0))

test42 = TestCase (assertEqual "WireAllocMap: initialization (5/5)."
                               Undeclared
                               (getDeclType "decl5" alloc0))

test43 = TestCase (assertEqual "WireAllocMap: allocation (1/20)."
                               (Scalar QWire)
                               (getDeclType "decl1" alloc1))

test44 = TestCase (assertEqual "WireAllocMap: allocation (2/20)."
                               Undeclared
                               (getDeclType "decl2" alloc1))

test45 = TestCase (assertEqual "WireAllocMap: allocation (3/20)."
                               Undeclared
                               (getDeclType "decl3" alloc1))

test46 = TestCase (assertEqual "WireAllocMap: allocation (4/20)."
                               Undeclared
                               (getDeclType "decl4" alloc1))

test47 = TestCase (assertEqual "WireAllocMap: allocation (5/20)."
                               Undeclared
                               (getDeclType "decl5" alloc1))

test48 = TestCase (assertEqual "WireAllocMap: allocation (6/20)."
                               (Scalar QWire)
                               (getDeclType "decl1" alloc2))

test49 = TestCase (assertEqual "WireAllocMap: allocation (7/20)."
                               (Array CWire 5)
                               (getDeclType "decl2" alloc2))

test50 = TestCase (assertEqual "WireAllocMap: allocation (8/20)."
                               Undeclared
                               (getDeclType "decl3" alloc2))

test51 = TestCase (assertEqual "WireAllocMap: allocation (9/20)."
                               Undeclared
                               (getDeclType "decl4" alloc2))

test52 = TestCase (assertEqual "WireAllocMap: allocation (10/20)."
                               Undeclared
                               (getDeclType "decl5" alloc2))

test53 = TestCase (assertEqual "WireAllocMap: allocation (11/20)."
                               (Scalar QWire)
                               (getDeclType "decl1" alloc3))

test54 = TestCase (assertEqual "WireAllocMap: allocation (12/20)."
                               (Array CWire 5)
                               (getDeclType "decl2" alloc3))

test55 = TestCase (assertEqual "WireAllocMap: allocation (13/20)."
                               (Array QWire 4)
                               (getDeclType "decl3" alloc3))

test56 = TestCase (assertEqual "WireAllocMap: allocation (14/20)."
                               Undeclared
                               (getDeclType "decl4" alloc3))

test57 = TestCase (assertEqual "WireAllocMap: allocation (15/20)."
                               Undeclared
                               (getDeclType "decl5" alloc3))

test58 = TestCase (assertEqual "WireAllocMap: allocation (16/20)."
                               (Scalar QWire)
                               (getDeclType "decl1" alloc4))

test59 = TestCase (assertEqual "WireAllocMap: allocation (17/20)."
                               (Array CWire 5)
                               (getDeclType "decl2" alloc4))

test60 = TestCase (assertEqual "WireAllocMap: allocation (18/20)."
                               (Array QWire 4)
                               (getDeclType "decl3" alloc4))

test61 = TestCase (assertEqual "WireAllocMap: allocation (19/20)."
                               (Scalar CWire)
                               (getDeclType "decl4" alloc4))

test62 = TestCase (assertEqual "WireAllocMap: allocation (20/20)."
                               Undeclared
                               (getDeclType "decl5" alloc4))

test63 = TestCase (assertEqual "WireAllocMap: double initialize (1/6)."
                               Nothing
                               (allocate QWire "decl1" Nothing alloc1))

test64 = TestCase (assertEqual "WireAllocMap: double initialize (2/6)."
                               Nothing
                               (allocate QWire "decl1" Nothing alloc2))

test65 = TestCase (assertEqual "WireAllocMap: double initialize (3/6)."
                               Nothing
                               (allocate QWire "decl1" Nothing alloc4))

test66 = TestCase (assertEqual "WireAllocMap: double initialize (4/6)."
                               Nothing
                               (allocate QWire "decl2" Nothing alloc4))

test67 = TestCase (assertEqual "WireAllocMap: double initialize (5/6)."
                               Nothing
                               (allocate QWire "decl3" Nothing alloc4))

test68 = TestCase (assertEqual "WireAllocMap: double initialize (6/6)."
                               Nothing
                               (allocate QWire "decl4" Nothing alloc4))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "WireAPI_UniqueStates_1" test1,
                                     TestLabel "WireAPI_UniqueStates_2" test2,
                                     TestLabel "WireAPI_UniqueStates_3" test3,
                                     TestLabel "WireAPI_UniqueStates_4" test4,
                                     TestLabel "WireAPI_UniqueStates_5" test5,
                                     TestLabel "WireAPI_UniqueStates_6" test6,
                                     TestLabel "WireAPI_UniqueStates_7" test7,
                                     TestLabel "WireAPI_UniqueStates_8" test8,
                                     TestLabel "WireAPI_UniqueStates_9" test9,
                                     TestLabel "WireAPI_UniqueStates_10" test10,
                                     TestLabel "WireAPI_Errors_1" test11,
                                     TestLabel "WireAPI_Errors_2" test12,
                                     TestLabel "WireAPI_Errors_3" test13,
                                     TestLabel "WireAPI_Errors_4" test14,
                                     TestLabel "WireAPI_Errors_5" test15,
                                     TestLabel "WireAPI_Errors_6" test16,
                                     TestLabel "WireAPI_Transitions_1" test17,
                                     TestLabel "WireAPI_Transitions_2" test18,
                                     TestLabel "WireAPI_Transitions_3" test19,
                                     TestLabel "WireAPI_Transitions_4" test20,
                                     TestLabel "WireAPI_Transitions_5" test21,
                                     TestLabel "WireAPI_Output_1" test22,
                                     TestLabel "WireAPI_Output_2" test23,
                                     TestLabel "WireAPI_Output_3" test24,
                                     TestLabel "WireAPI_Output_4" test25,
                                     TestLabel "WireAPI_Output_5" test26,
                                     TestLabel "WireAPI_Input_1" test27,
                                     TestLabel "WireAPI_Input_2" test28,
                                     TestLabel "WireAPI_Input_3" test29,
                                     TestLabel "WireAPI_Input_4" test30,
                                     TestLabel "WireAPI_Input_5" test31,
                                     TestLabel "WireAPI_WireIndex_1" test32,
                                     TestLabel "WireAPI_WireIndex_2" test33,
                                     TestLabel "WireAPI_WireIndex_3" test34,
                                     TestLabel "WireAPI_WireIndex_4" test35,
                                     TestLabel "WireAPI_WireIndex_5" test36,
                                     TestLabel "WireAPI_WireIndex_6" test37,
                                     TestLabel "WireAllocMap_Init_1" test38,
                                     TestLabel "WireAllocMap_Init_2" test39,
                                     TestLabel "WireAllocMap_Init_3" test40,
                                     TestLabel "WireAllocMap_Init_4" test41,
                                     TestLabel "WireAllocMap_Init_5" test42,
                                     TestLabel "WireAllocMap_Alloc_1" test43,
                                     TestLabel "WireAllocMap_Alloc_2" test44,
                                     TestLabel "WireAllocMap_Alloc_3" test45,
                                     TestLabel "WireAllocMap_Alloc_4" test46,
                                     TestLabel "WireAllocMap_Alloc_5" test47,
                                     TestLabel "WireAllocMap_Alloc_6" test48,
                                     TestLabel "WireAllocMap_Alloc_7" test49,
                                     TestLabel "WireAllocMap_Alloc_8" test50,
                                     TestLabel "WireAllocMap_Alloc_9" test51,
                                     TestLabel "WireAllocMap_Alloc_10" test52,
                                     TestLabel "WireAllocMap_Alloc_11" test53,
                                     TestLabel "WireAllocMap_Alloc_12" test54,
                                     TestLabel "WireAllocMap_Alloc_13" test55,
                                     TestLabel "WireAllocMap_Alloc_14" test56,
                                     TestLabel "WireAllocMap_Alloc_15" test57,
                                     TestLabel "WireAllocMap_Alloc_16" test58,
                                     TestLabel "WireAllocMap_Alloc_17" test59,
                                     TestLabel "WireAllocMap_Alloc_18" test60,
                                     TestLabel "WireAllocMap_Alloc_19" test61,
                                     TestLabel "WireAllocMap_Alloc_20" test62,
                                     TestLabel "WireAllocMap_Unique_1" test63,
                                     TestLabel "WireAllocMap_Unique_2" test64,
                                     TestLabel "WireAllocMap_Unique_3" test65,
                                     TestLabel "WireAllocMap_Unique_4" test66,
                                     TestLabel "WireAllocMap_Unique_5" test67,
                                     TestLabel "WireAllocMap_Unique_6" test68]

main = defaultMain tests
