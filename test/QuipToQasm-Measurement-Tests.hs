module Main where

import qualified Data.IntMap.Strict as IntMap
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.AST
import LinguaQuanta.Qasm.Operand
import LinguaQuanta.Quip.Wire
import LinguaQuanta.QuipToQasm.Measurement
import LinguaQuanta.QuipToQasm.Wire

-----------------------------------------------------------------------------------------
-- translateMeasurement

inputs = [(1, QWire), (5, QWire), (3, CWire), (23, QWire), (50, QWire), (44, CWire)]
allocs = allocateInputWires $ IntMap.fromList inputs

(collapsedAllocs1, measStmts1) = translateMeasurement allocs 1
(collapsedAllocs2, measStmts2) = translateMeasurement collapsedAllocs1 50

test1 = TestCase (assertEqual "translateMeasurement returns the correct stmts (1/2)."
                              expected
                              measStmts1)
    where targcell = Cell "input_qwires" 0
          expected = [AstBitDecl Nothing "shadow_cwire_0",
                      AstAssign "shadow_cwire_0" Nothing $ QuipMeasure targcell]

test2 = TestCase (assertEqual "translateMeasurement returns the correct stmts (2/2)."
                              expected
                              measStmts2)
    where targcell = Cell "input_qwires" 3
          expected = [AstBitDecl Nothing "shadow_cwire_1",
                      AstAssign "shadow_cwire_1" Nothing $ QuipMeasure targcell]

test3 = TestCase (assertEqual "translateMeasurement collapses state (1/3)."
                              (Just CWire :: Maybe WireType)
                              (getState 1 collapsedAllocs1))

test4 = TestCase (assertEqual "translateMeasurement collapses state (2/3)."
                              (Just CWire :: Maybe WireType)
                              (getState 1 collapsedAllocs2))

test5 = TestCase (assertEqual "translateMeasurement collapses state (3/3)."
                              (Just CWire :: Maybe WireType)
                              (getState 50 collapsedAllocs2))

test6 = TestCase (assertEqual "translateMeasurement leaves other wires unchanged (1/4)."
                              (Just QWire :: Maybe WireType)
                              (getState 5 collapsedAllocs2))

test7 = TestCase (assertEqual "translateMeasurement leaves other wires unchanged (2/4)."
                              (Just CWire :: Maybe WireType)
                              (getState 3 collapsedAllocs2))

test8 = TestCase (assertEqual "translateMeasurement leaves other wires unchanged (3/4)."
                              (Just QWire :: Maybe WireType)
                              (getState 23 collapsedAllocs2))

test9 = TestCase (assertEqual "translateMeasurement leaves other wires unchanged (4/4)."
                              (Just CWire :: Maybe WireType)
                              (getState 44 collapsedAllocs2))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "translateMeasure_Stmts_1" test1,
                                     TestLabel "translateMeasure_Stmts_2" test2,
                                     TestLabel "translateMeasure_Collapse_1" test3,
                                     TestLabel "translateMeasure_Collapse_2" test4,
                                     TestLabel "translateMeasure_Collapse_3" test5,
                                     TestLabel "translateMeausre_Unchanged_1" test6,
                                     TestLabel "translateMeausre_Unchanged_2" test7,
                                     TestLabel "translateMeausre_Unchanged_3" test8,
                                     TestLabel "translateMeausre_Unchanged_4" test9]

main = defaultMain tests
