module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
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
                                     TestLabel "WireAPI_WireIndex_6" test37]

main = defaultMain tests
