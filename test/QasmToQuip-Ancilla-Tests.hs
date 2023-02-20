module Main where

import qualified Data.IntMap.Strict as IntMap
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.Operand
import LinguaQuanta.QasmToQuip.Ancilla
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
-- simple tests

test1 = TestCase (assertEqual "translateAncilla"
                              [CDiscardGate 1]
                              (translateAncilla allocs (QRef "var2") CDiscardGate))

test2 = TestCase (assertEqual "updateWireMap"
                              9
                              (IntMap.size $ toQuipperOutputs allocs'))
    where fns     = (termScalar, termCell)
          allocs' = updateWireMap allocs (QRef "var2") fns

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "translateAncilla" test1,
                                     TestLabel "updateWireMap" test2]

main = defaultMain tests
