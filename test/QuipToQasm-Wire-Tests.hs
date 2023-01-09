module Main where

import qualified Data.IntMap.Strict as IntMap
import Data.Maybe
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.Gate
import LinguaQuanta.Quip.Wire
import LinguaQuanta.QuipToQasm.Wire

-----------------------------------------------------------------------------------------
-- Tests allocation and lookup

mixedWires = [(1, QWire),
              (5, QWire),
              (3, CWire),
              (9, CWire),
              (23, QWire),
              (50, QWire),
              (44, CWire)]

allocEmpty = allocateInputWires IntMap.empty
allocMixed = allocateInputWires $ IntMap.fromList mixedWires

test1 = TestCase (assertEqual "WireLookup retrieval from empty wire map (1/7)."
                              (Nothing :: Maybe Operand)
                              (getAllocation QWire 1 allocEmpty))

test2 = TestCase (assertEqual "WireLookup retrieval from empty wire map (2/7)."
                              (Nothing :: Maybe Operand)
                              (getAllocation QWire 5 allocEmpty))

test3 = TestCase (assertEqual "WireLookup retrieval from empty wire map (3/7)."
                              (Nothing :: Maybe Operand)
                              (getAllocation QWire 23 allocEmpty))

test4 = TestCase (assertEqual "WireLookup retrieval from empty wire map (4/7)."
                              (Nothing :: Maybe Operand)
                              (getAllocation QWire 50 allocEmpty))

test5 = TestCase (assertEqual "WireLookup retrieval from empty wire map (5/7)."
                              (Nothing :: Maybe Operand)
                              (getAllocation CWire 3 allocEmpty))

test6 = TestCase (assertEqual "WireLookup retrieval from empty wire map (6/7)."
                              (Nothing :: Maybe Operand)
                              (getAllocation CWire 9 allocEmpty))

test7 = TestCase (assertEqual "WireLookup retrieval from empty wire map (7/7)."
                              (Nothing :: Maybe Operand)
                              (getAllocation CWire 44 allocEmpty))

test8 = TestCase (assertEqual "WireLookup retrieval from empty wire map (1/7)."
                              expt
                              (getAllocation QWire 1 allocMixed))
    where expt = Just (Cell "input_qwires" 0) :: Maybe Operand

test9 = TestCase (assertEqual "WireLookup retrieval from empty wire map (2/7)."
                              expt
                              (getAllocation QWire 5 allocMixed))
    where expt = Just (Cell "input_qwires" 1) :: Maybe Operand

test10 = TestCase (assertEqual "WireLookup retrieval from empty wire map (3/7)."
                               expt
                               (getAllocation QWire 23 allocMixed))
    where expt = Just (Cell "input_qwires" 2) :: Maybe Operand

test11 = TestCase (assertEqual "WireLookup retrieval from empty wire map (4/7)."
                                expt
                               (getAllocation QWire 50 allocMixed))
    where expt = Just (Cell "input_qwires" 3) :: Maybe Operand

test12 = TestCase (assertEqual "WireLookup retrieval from empty wire map (5/7)."
                               expt
                               (getAllocation CWire 3 allocMixed))
    where expt = Just (Cell "input_cwires" 0) :: Maybe Operand

test13 = TestCase (assertEqual "WireLookup retrieval from empty wire map (6/7)."
                               expt
                               (getAllocation CWire 9 allocMixed))
    where expt = Just (Cell "input_cwires" 1) :: Maybe Operand

test14 = TestCase (assertEqual "WireLookup retrieval from empty wire map (7/7)."
                               expt
                               (getAllocation CWire 44 allocMixed))
    where expt = Just (Cell "input_cwires" 2) :: Maybe Operand

test15 = TestCase (assertEqual "WireLookup retrieval fails with invalid type."
                               (Nothing :: Maybe Operand)
                               (getAllocation CWire 1 allocMixed))

test16 = TestCase (assertEqual "WireLookup retrieval fails with invalid identifier."
                               (Nothing :: Maybe Operand)
                               (getAllocation QWire 256 allocMixed))

-----------------------------------------------------------------------------------------
-- getState: After allocation

test17 = TestCase (assertEqual "getState supports QWire allocations."
                               (Just QWire :: Maybe WireType)
                               (getState 1 allocMixed))

test18 = TestCase (assertEqual "getState supports CWire allocations."
                               (Just CWire :: Maybe WireType)
                               (getState 9 allocMixed))

test19 = TestCase (assertEqual "getState handles missing allocations."
                               (Nothing :: Maybe WireType)
                               (getState 256 allocMixed))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "Empty_Allocation_1" test1,
                                     TestLabel "Empty_Allocation_2" test2,
                                     TestLabel "Empty_Allocation_3" test3,
                                     TestLabel "Empty_Allocation_4" test4,
                                     TestLabel "Empty_Allocation_5" test5,
                                     TestLabel "Empty_Allocation_6" test6,
                                     TestLabel "Empty_Allocation_7" test7,
                                     TestLabel "Mixed_Allocation_1" test8,
                                     TestLabel "Mixed_Allocation_2" test9,
                                     TestLabel "Mixed_Allocation_3" test10,
                                     TestLabel "Mixed_Allocation_4" test11,
                                     TestLabel "Mixed_Allocation_5" test12,
                                     TestLabel "Mixed_Allocation_6" test13,
                                     TestLabel "Mixed_Allocation_7" test14,
                                     TestLabel "Mixed_Allocation_BadType" test15,
                                     TestLabel "Mixed_Allocation_BadID" test16,
                                     TestLabel "getState_QWire" test17,
                                     TestLabel "getState_CWire" test18,
                                     TestLabel "getState_BadID" test19]

main = defaultMain tests
