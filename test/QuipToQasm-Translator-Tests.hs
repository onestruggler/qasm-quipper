module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Qasm.AST
import Quip.Parser
import QuipToQasm.Translator

import qualified Data.IntMap.Strict as IntMap

-------------------------------------------------------------------------------
-- * Wire Tests

iomap_1 = IntMap.empty
iomap_2 = IntMap.fromList[(1, QWire)]
iomap_3 = IntMap.fromList[(1, QWire), (3, QWire), (5, QWire)]

make_wire_circ :: IntMap.IntMap WireType -> IntMap.IntMap WireType -> GateCirc
make_wire_circ ins outs = GateCirc { inputs  = ins
                                   , gates   = []
                                   , outputs = outs
                                   , size    = IntMap.size ins + IntMap.size outs
                                   }

test1 = TestCase (assertEqual "Can convert empty circuit."
                              []
                              (translate $ make_wire_circ iomap_1 iomap_1))

test2 = TestCase (assertEqual "Can convert a 1-to-0 qubit circuit."
                              [AstQubitDecl (Just 1) "input_qwires"]
                              (translate $ make_wire_circ iomap_2 iomap_1))

test3 = TestCase (assertEqual "Can convert a 1-to-1 qubit circuit."
                              [AstQubitDecl (Just 1) "input_qwires"]
                              (translate $ make_wire_circ iomap_2 iomap_2))

test4 = TestCase (assertEqual "Can convert a 3-to-0 qubit circuit."
                              [AstQubitDecl (Just 3) "input_qwires"]
                              (translate $ make_wire_circ iomap_3 iomap_1))

test5 = TestCase (assertEqual "Can convert a 3-to-1 qubit circuit."
                              [AstQubitDecl (Just 3) "input_qwires"]
                              (translate $ make_wire_circ iomap_3 iomap_2))

test6 = TestCase (assertEqual "Can convert a 3-to-3 qubit circuit."
                              [AstQubitDecl (Just 3) "input_qwires"]
                              (translate $ make_wire_circ iomap_3 iomap_3))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "Empty_Circ_0_to_0" test1,
                                     TestLabel "Empty_Circ_1_to_0" test2,
                                     TestLabel "Empty_Circ_1_to_1" test3,
                                     TestLabel "Empty_Circ_3_to_0" test4,
                                     TestLabel "Empty_Circ_3_to_1" test5,
                                     TestLabel "Empty_Circ_3_to_3" test6]

main = defaultMain tests
