module Main where

import qualified Data.IntMap.Strict as IntMap
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.Operand
import LinguaQuanta.QasmToQuip.Operand
import LinguaQuanta.QasmToQuip.Wire
import LinguaQuanta.Quip.Wire

-----------------------------------------------------------------------------------------
-- toOperand

test1 = TestCase (assertEqual "toOperand: no index (1/2)."
                              (QRef "var1")
                              (toOperand "var1" Nothing))

test2 = TestCase (assertEqual "toOperand: no index (2/2)."
                              (QRef "var2")
                              (toOperand "var2" Nothing))

test3 = TestCase (assertEqual "toOperand: with index (1/2)."
                              (Cell "var1" 2)
                              (toOperand "var1" $ Just 2))

test4 = TestCase (assertEqual "toOperand: with index (2/2)."
                              (Cell "var2" 5)
                              (toOperand "var2" $ Just 5))

-----------------------------------------------------------------------------------------
-- getWire

(Just allocsTmp0) = allocate QWire "var1" Nothing initialAllocations
(Just allocsTmp1) = allocate CWire "var2" Nothing allocsTmp0
(Just allocsTmp2) = allocate QWire "var3" (Just 3) allocsTmp1
(Just allocs)     = allocate QWire "var4" (Just 5) allocsTmp2

test5 = TestCase (assertEqual "getWire: with QRef (1/2)."
                              (Just 0)
                              (getWire (QRef "var1") allocs))

test6 = TestCase (assertEqual "getWire: with QRef (2/2)."
                              (Just 1)
                              (getWire (QRef "var2") allocs))

test7 = TestCase (assertEqual "getWire: with Cell (1/2)."
                              (Just 3)
                              (getWire (Cell "var3" 1) allocs))

test8 = TestCase (assertEqual "getWire: with Cell (2/2)."
                              (Just 7)
                              (getWire (Cell "var4" 2) allocs))

-----------------------------------------------------------------------------------------
-- getUpdateFn

(Just allocUpdate1) = getUpdateFn (initScalar, initCell) (QRef "var1") allocs
(Just allocUpdate2) = getUpdateFn (termScalar, termCell) (Cell "var3" 1) allocs

test9 = TestCase (assertEqual "getUpdateFn with QRef."
                               9
                               (IntMap.size $ toQuipperInputs allocUpdate1))

test10 = TestCase (assertEqual "getUpdateFn with Cell."
                               9
                               (IntMap.size $ toQuipperOutputs allocUpdate2))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "getOperand_QRef_1" test1,
                                     TestLabel "getOperand_QRef_2" test2,
                                     TestLabel "getOperand_Cell_1" test3,
                                     TestLabel "getOperand_Cell_2" test4,
                                     TestLabel "getWire_QRef_1" test5,
                                     TestLabel "getWire_QRef_2" test6,
                                     TestLabel "getWire_Cell_1" test7,
                                     TestLabel "getWire_Cell_2" test8,
                                     TestLabel "getUpdateFn_QRef" test9,
                                     TestLabel "getUpdateFn_Cell" test10]

main = defaultMain tests
