module Main where

import qualified Data.IntMap.Strict as IntMap
import Data.Maybe
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Quip.Wire
import LinguaQuanta.QasmToQuip.Wire

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
-- WireAllocMaps to Quipper IO Without Wire Interactions.

io0 = IntMap.empty :: IntMap.IntMap WireType
io1 = IntMap.insert 0 QWire io0
io2 = IntMap.insert 1 CWire
    $ IntMap.insert 2 CWire
    $ IntMap.insert 3 CWire
    $ IntMap.insert 4 CWire
    $ IntMap.insert 5 CWire io1
io3 = IntMap.insert 6 QWire
    $ IntMap.insert 7 QWire
    $ IntMap.insert 8 QWire
    $ IntMap.insert 9 QWire io2
io4 = IntMap.insert 10 CWire io3

test69 = TestCase (assertEqual "WireAllocMap: basic input cases (1/5)."
                               io0
                               (toQuipperInputs alloc0))

test70 = TestCase (assertEqual "WireAllocMap: basic input cases (2/5)."
                               io1
                               (toQuipperInputs alloc1))

test71 = TestCase (assertEqual "WireAllocMap: basic input cases (3/5)."
                               io2
                               (toQuipperInputs alloc2))

test72 = TestCase (assertEqual "WireAllocMap: basic input cases (4/5)."
                               io3
                               (toQuipperInputs alloc3))

test73 = TestCase (assertEqual "WireAllocMap: basic input cases (5/5)."
                               io4
                               (toQuipperInputs alloc4))

test74 = TestCase (assertEqual "WireAllocMap: basic output cases (1/5)."
                               io0
                               (toQuipperOutputs alloc0))

test75 = TestCase (assertEqual "WireAllocMap: basic output cases (2/5)."
                               io1
                               (toQuipperOutputs alloc1))

test76 = TestCase (assertEqual "WireAllocMap: basic output cases (3/5)."
                               io2
                               (toQuipperOutputs alloc2))

test77 = TestCase (assertEqual "WireAllocMap: basic output cases (4/5)."
                               io3
                               (toQuipperOutputs alloc3))

test78 = TestCase (assertEqual "WireAllocMap: basic output cases (5/5)."
                               io4
                               (toQuipperOutputs alloc4))

-----------------------------------------------------------------------------------------
-- Scalar Updates to WireAllocMaps with Quipper IO.

-- QWire case.
scalarRes1 = fromJust $ initScalar "decl1" alloc4
scalarRes2 = fromJust $ termScalar "decl1" alloc4
scalarRes3 = fromJust $ useScalar  "decl1" alloc4
scalarRes4 = fromJust $ termScalar "decl1" scalarRes1

scalarIn1 = IntMap.delete 0 io4
scalarIn2 = io4
scalarIn3 = io4
scalarIn4 = scalarIn1

scalarOut1 = io4
scalarOut2 = IntMap.delete 0 io4
scalarOut3 = io4
scalarOut4 = IntMap.delete 0 scalarOut1

test79 = TestCase (assertEqual "WireAllocMap: qwire scalar inputs updates (1/4)."
                               scalarIn1
                               (toQuipperInputs scalarRes1))

test80 = TestCase (assertEqual "WireAllocMap: qwire scalar inputs updates (2/4)."
                               scalarIn2
                               (toQuipperInputs scalarRes2))

test81 = TestCase (assertEqual "WireAllocMap: qwire scalar inputs updates (3/4)."
                               scalarIn3
                               (toQuipperInputs scalarRes3))

test82 = TestCase (assertEqual "WireAllocMap: qwire scalar inputs updates (4/4)."
                               scalarIn4
                               (toQuipperInputs scalarRes4))

test83 = TestCase (assertEqual "WireAllocMap: qwire scalar output updates (1/4)."
                               scalarOut1
                               (toQuipperOutputs scalarRes1))

test84 = TestCase (assertEqual "WireAllocMap: qwire scalar output updates (2/4)."
                               scalarOut2
                               (toQuipperOutputs scalarRes2))

test85 = TestCase (assertEqual "WireAllocMap: qwire scalar output updates (3/4)."
                               scalarOut3
                               (toQuipperOutputs scalarRes3))

test86 = TestCase (assertEqual "WireAllocMap: qwire scalar output updates (4/4)."
                               scalarOut4
                               (toQuipperOutputs scalarRes4))

-- CWire case.
scalarRes5 = fromJust $ initScalar "decl4" scalarRes4
scalarRes6 = fromJust $ termScalar "decl4" scalarRes4
scalarRes7 = fromJust $ useScalar  "decl4" scalarRes4
scalarRes8 = fromJust $ termScalar "decl4" scalarRes5

scalarIn5 = IntMap.delete 10 scalarIn4
scalarIn6 = scalarIn4
scalarIn7 = scalarIn4
scalarIn8 = scalarIn5

scalarOut5 = scalarOut4
scalarOut6 = IntMap.delete 10 scalarOut4
scalarOut7 = scalarOut4
scalarOut8 = IntMap.delete 10 scalarOut5

test87 = TestCase (assertEqual "WireAllocMap: cwire scalar inputs updates (1/4)."
                               scalarIn5
                               (toQuipperInputs scalarRes5))

test88 = TestCase (assertEqual "WireAllocMap: cwire scalar inputs updates (2/4)."
                               scalarIn6
                               (toQuipperInputs scalarRes6))

test89 = TestCase (assertEqual "WireAllocMap: cwire scalar inputs updates (3/4)."
                               scalarIn7
                               (toQuipperInputs scalarRes7))

test90 = TestCase (assertEqual "WireAllocMap: cwire scalar inputs updates (4/4)."
                               scalarIn8
                               (toQuipperInputs scalarRes8))

test91 = TestCase (assertEqual "WireAllocMap: cwire scalar output updates (1/4)."
                               scalarOut5
                               (toQuipperOutputs scalarRes5))

test92 = TestCase (assertEqual "WireAllocMap: cwire scalar output updates (2/4)."
                               scalarOut6
                               (toQuipperOutputs scalarRes6))

test93 = TestCase (assertEqual "WireAllocMap: cwire scalar output updates (3/4)."
                               scalarOut7
                               (toQuipperOutputs scalarRes7))

test94 = TestCase (assertEqual "WireAllocMap: cwire scalar output updates (4/4)."
                               scalarOut8
                               (toQuipperOutputs scalarRes8))

-- Invalid uses.

test95 = TestCase (assertEqual "WireAllocMap: cannot apply scalar updates to arrays."
                               Nothing
                               (initScalar "decl2" alloc4))

test96 = TestCase (assertEqual "WireAllocMap: scalar updates require allocated vars."
                               Nothing
                               (initScalar "decl5" alloc4))

-----------------------------------------------------------------------------------------
-- Array Updates to WireAllocMaps with Quipper IO.

-- QWire case.
arrayRes1 = fromJust $ initCell "decl3" 2 alloc4
arrayRes2 = fromJust $ termCell "decl3" 2 alloc4
arrayRes3 = fromJust $ useCell  "decl3" 2 alloc4
arrayRes4 = fromJust $ termCell "decl3" 2 arrayRes1

arrayIn1 = IntMap.delete 8 io4
arrayIn2 = io4
arrayIn3 = io4
arrayIn4 = arrayIn1

arrayOut1 = io4
arrayOut2 = IntMap.delete 8 io4
arrayOut3 = io4
arrayOut4 = IntMap.delete 8 arrayOut1

test97 = TestCase (assertEqual "WireAllocMap: qwire array inputs updates (1/4)."
                               arrayIn1
                               (toQuipperInputs arrayRes1))

test98 = TestCase (assertEqual "WireAllocMap: qwire array inputs updates (2/4)."
                               arrayIn2
                               (toQuipperInputs arrayRes2))

test99 = TestCase (assertEqual "WireAllocMap: qwire array inputs updates (3/4)."
                               arrayIn3
                               (toQuipperInputs arrayRes3))

test100 = TestCase (assertEqual "WireAllocMap: qwire array inputs updates (4/4)."
                                arrayIn4
                                (toQuipperInputs arrayRes4))

test101 = TestCase (assertEqual "WireAllocMap: qwire array output updates (1/4)."
                                arrayOut1
                                (toQuipperOutputs arrayRes1))

test102 = TestCase (assertEqual "WireAllocMap: qwire array output updates (2/4)."
                                arrayOut2
                                (toQuipperOutputs arrayRes2))

test103 = TestCase (assertEqual "WireAllocMap: qwire array output updates (3/4)."
                                arrayOut3
                                (toQuipperOutputs arrayRes3))

test104 = TestCase (assertEqual "WireAllocMap: qwire array output updates (4/4)."
                                arrayOut4
                                (toQuipperOutputs arrayRes4))

-- CWire case.
arrayRes5 = fromJust $ initCell "decl2" 3 arrayRes4
arrayRes6 = fromJust $ termCell "decl2" 3 arrayRes4
arrayRes7 = fromJust $ useCell  "decl2" 3 arrayRes4
arrayRes8 = fromJust $ termCell "decl2" 3 arrayRes5

arrayIn5 = IntMap.delete 4 arrayIn4
arrayIn6 = arrayIn4
arrayIn7 = arrayIn4
arrayIn8 = arrayIn5

arrayOut5 = arrayOut4
arrayOut6 = IntMap.delete 4 arrayOut4
arrayOut7 = arrayOut4
arrayOut8 = IntMap.delete 4 arrayOut5

test105 = TestCase (assertEqual "WireAllocMap: cwire array inputs updates (1/4)."
                                arrayIn5
                                (toQuipperInputs arrayRes5))

test106 = TestCase (assertEqual "WireAllocMap: cwire array inputs updates (2/4)."
                                arrayIn6
                                (toQuipperInputs arrayRes6))

test107 = TestCase (assertEqual "WireAllocMap: cwire array inputs updates (3/4)."
                                arrayIn7
                                (toQuipperInputs arrayRes7))

test108 = TestCase (assertEqual "WireAllocMap: cwire array inputs updates (4/4)."
                                arrayIn8
                                (toQuipperInputs arrayRes8))

test109 = TestCase (assertEqual "WireAllocMap: cwire array output updates (1/4)."
                                arrayOut5
                                (toQuipperOutputs arrayRes5))

test110 = TestCase (assertEqual "WireAllocMap: cwire array output updates (2/4)."
                                arrayOut6
                                (toQuipperOutputs arrayRes6))

test111 = TestCase (assertEqual "WireAllocMap: cwire array output updates (3/4)."
                                arrayOut7
                                (toQuipperOutputs arrayRes7))

test112 = TestCase (assertEqual "WireAllocMap: cwire array output updates (4/4)."
                                arrayOut8
                                (toQuipperOutputs arrayRes8))

-- Invalid uses.

test113 = TestCase (assertEqual "WireAllocMap: cannot apply array updates to scalars."
                                Nothing
                                (initCell "decl1" 0 alloc4))

test114 = TestCase (assertEqual "WireAllocMap: array updates require allocated vars."
                                Nothing
                                (initCell "decl5" 0 alloc4))

test115 = TestCase (assertEqual "WireAllocMap: array updates require in-bounds index."
                                Nothing
                                (initCell "decl2" 10 alloc4))

-----------------------------------------------------------------------------------------
-- toSize: fresh allocations

test116 = TestCase (assertEqual "WireAllocMap: toSize on fresh allocations (1/5)."
                                0
                                (toSize alloc0))

test117 = TestCase (assertEqual "WireAllocMap: toSize on fresh allocations (2/5)."
                                1
                                (toSize alloc1))

test118 = TestCase (assertEqual "WireAllocMap: toSize on fresh allocations (3/5)."
                                6
                                (toSize alloc2))

test119 = TestCase (assertEqual "WireAllocMap: toSize on fresh allocations (4/5)."
                                10
                                (toSize alloc3))

test120 = TestCase (assertEqual "WireAllocMap: toSize on fresh allocations (5/5)."
                                11
                                (toSize alloc4))

-----------------------------------------------------------------------------------------
-- toSize: example trace

trace1 = fromJust $ initScalar "decl1"   alloc2
trace2 = fromJust $ termCell   "decl2" 1 trace1
trace3 = fromJust $ termCell   "decl2" 3 trace2

trace4 = fromJust $ allocate QWire "decl3" (Just 4) trace3

trace5 = fromJust $ initCell "decl2" 1 trace4
trace6 = fromJust $ termCell "decl3" 0 trace5
trace7 = fromJust $ useCell  "decl3" 2 trace6

trace8 = fromJust $ allocate QWire "decl4" (Just 10) trace7

test121 = TestCase (assertEqual "WireAllocMap: toSize on a trace of updates (1/8)."
                                6
                                (toSize trace1))

test122 = TestCase (assertEqual "WireAllocMap: toSize on a trace of updates (2/8)."
                                6
                                (toSize trace2))

test123 = TestCase (assertEqual "WireAllocMap: toSize on a trace of updates (3/8)."
                                6
                                (toSize trace3))

test124 = TestCase (assertEqual "WireAllocMap: toSize on a trace of updates (4/8)."
                                8
                                (toSize trace4))

test125 = TestCase (assertEqual "WireAllocMap: toSize on a trace of updates (5/8)."
                                9
                                (toSize trace5))

test126 = TestCase (assertEqual "WireAllocMap: toSize on a trace of updates (6/8)."
                                9
                                (toSize trace6))

test127 = TestCase (assertEqual "WireAllocMap: toSize on a trace of updates (7/8)."
                                9
                                (toSize trace7))

test128 = TestCase (assertEqual "WireAllocMap: toSize on a trace of updates (8/8)."
                                18
                                (toSize trace8))

-----------------------------------------------------------------------------------------
-- Tests basic wire index queries

test129 = TestCase (assertEqual "getScalarIndex: allocated wire (1/2)."
                                (Just 0 :: Maybe Int)
                                (getScalarIndex "decl1" alloc4))

test130 = TestCase (assertEqual "getScalarIndex: allocated wire (2/2)."
                                (Just 10 :: Maybe Int)
                                (getScalarIndex "decl4" alloc4))

test131 = TestCase (assertEqual "getScalarIndex: fails for undeclared declarations."
                                (Nothing :: Maybe Int)
                                (getScalarIndex "decl6" alloc4))

test132 = TestCase (assertEqual "getScalarIndex: fails for array declarations."
                                (Nothing :: Maybe Int)
                                (getScalarIndex "decl2" alloc4))

test133 = TestCase (assertEqual "getCellIndex: allocated wire (1/2)."
                                (Just 3 :: Maybe Int)
                                (getCellIndex "decl2" 2 alloc4))

test134 = TestCase (assertEqual "getCellIndex: allocated wire (2/2)."
                                (Just 7 :: Maybe Int)
                                (getCellIndex "decl3" 1 alloc4))

test135 = TestCase (assertEqual "getCellIndex: fails for undeclared declarations."
                                (Nothing :: Maybe Int)
                                (getCellIndex "decl6" 0 alloc4))

test136 = TestCase (assertEqual "getCellIndex: fails for scalar declarations."
                                (Nothing :: Maybe Int)
                                (getCellIndex "decl1" 0 alloc4))

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
                                     TestLabel "WireAllocMap_Unique_6" test68,
                                     TestLabel "WireAllocMap_BasicIn_1" test69,
                                     TestLabel "WireAllocMap_BasicIn_2" test70,
                                     TestLabel "WireAllocMap_BasicIn_3" test71,
                                     TestLabel "WireAllocMap_BasicIn_4" test72,
                                     TestLabel "WireAllocMap_BasicIn_5" test73,
                                     TestLabel "WireAllocMap_BasicOut_1" test74,
                                     TestLabel "WireAllocMap_BasicOut_2" test75,
                                     TestLabel "WireAllocMap_BasicOut_3" test76,
                                     TestLabel "WireAllocMap_BasicOut_4" test77,
                                     TestLabel "WireAllocMap_BasicOut_5" test78,
                                     TestLabel "WireAllocMap_QScalarIn_1" test79,
                                     TestLabel "WireAllocMap_QScalarIn_2" test80,
                                     TestLabel "WireAllocMap_QScalarIn_3" test81,
                                     TestLabel "WireAllocMap_QScalarIn_4" test82,
                                     TestLabel "WireAllocMap_QScalarOut_1" test83,
                                     TestLabel "WireAllocMap_QScalarOut_2" test84,
                                     TestLabel "WireAllocMap_QScalarOut_3" test85,
                                     TestLabel "WireAllocMap_QScalarOut_4" test86,
                                     TestLabel "WireAllocMap_CScalarIn_1" test87,
                                     TestLabel "WireAllocMap_CScalarIn_2" test88,
                                     TestLabel "WireAllocMap_CScalarIn_3" test89,
                                     TestLabel "WireAllocMap_CScalarIn_4" test90,
                                     TestLabel "WireAllocMap_CScalarOut_1" test91,
                                     TestLabel "WireAllocMap_CScalarOut_2" test92,
                                     TestLabel "WireAllocMap_CScalarOut_3" test93,
                                     TestLabel "WireAllocMap_CScalarOut_4" test94,
                                     TestLabel "WireAllocMap_ArrayAsScalarErr" test95,
                                     TestLabel "WireAllocMap_ScalarUndefErr" test96,
                                     TestLabel "WireAllocMap_QArrayIn_1" test97,
                                     TestLabel "WireAllocMap_QArrayIn_2" test98,
                                     TestLabel "WireAllocMap_QArrayIn_3" test99,
                                     TestLabel "WireAllocMap_QArrayIn_4" test100,
                                     TestLabel "WireAllocMap_QArrayOut_1" test101,
                                     TestLabel "WireAllocMap_QArrayOut_2" test102,
                                     TestLabel "WireAllocMap_QArrayOut_3" test103,
                                     TestLabel "WireAllocMap_QArrayOut_4" test104,
                                     TestLabel "WireAllocMap_CArrayIn_1" test105,
                                     TestLabel "WireAllocMap_CArrayIn_2" test106,
                                     TestLabel "WireAllocMap_CArrayIn_3" test107,
                                     TestLabel "WireAllocMap_CArrayIn_4" test108,
                                     TestLabel "WireAllocMap_CArrayOut_1" test109,
                                     TestLabel "WireAllocMap_CArrayOut_2" test110,
                                     TestLabel "WireAllocMap_CArrayOut_3" test111,
                                     TestLabel "WireAllocMap_CArrayOut_4" test112,
                                     TestLabel "WireAllocMap_ScalarAsArrayErr" test113,
                                     TestLabel "WireAllocMap_ArrayUndefErr" test114,
                                     TestLabel "WireAllocMap_BoundsErr" test115,
                                     TestLabel "WireAllocMap_toSize_Initial_1" test116,
                                     TestLabel "WireAllocMap_toSize_Initial_2" test117,
                                     TestLabel "WireAllocMap_toSize_Initial_3" test118,
                                     TestLabel "WireAllocMap_toSize_Initial_4" test119,
                                     TestLabel "WireAllocMap_toSize_Initial_5" test120,
                                     TestLabel "WireAllocMap_toSize_Trace_1" test121,
                                     TestLabel "WireAllocMap_toSize_Trace_2" test122,
                                     TestLabel "WireAllocMap_toSize_Trace_3" test123,
                                     TestLabel "WireAllocMap_toSize_Trace_4" test124,
                                     TestLabel "WireAllocMap_toSize_Trace_5" test125,
                                     TestLabel "WireAllocMap_toSize_Trace_6" test126,
                                     TestLabel "WireAllocMap_toSize_Trace_7" test127,
                                     TestLabel "WireAllocMap_toSize_Trace_8" test128,
                                     TestLabel "getScalarIndex_Valid_1" test129,
                                     TestLabel "getScalarIndex_Valid_2" test130,
                                     TestLabel "getScalarIndex_UndeclaredErr" test131,
                                     TestLabel "getScalarIndex_ArrayErr" test132,
                                     TestLabel "getCellIndex_Valid_1" test133,
                                     TestLabel "getCellIndex_Valid_2" test134,
                                     TestLabel "getCellIndex_UndeclaredErr" test135,
                                     TestLabel "getCellIndex_ScalarErr" test136]

main = defaultMain tests
