module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.AST
import LinguaQuanta.Qasm.Gate as Qasm
import LinguaQuanta.Qasm.GateName as Qasm
import LinguaQuanta.Qasm.Language
import LinguaQuanta.Quip.Gate as Quip
import LinguaQuanta.Quip.GateName as Quip
import LinguaQuanta.Quip.Quipper
import LinguaQuanta.QuipToQasm.Translator

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
-- Translating Unitaries

iomap_4 = IntMap.fromList [(0, QWire), (1, QWire), (2, QWire), (3, QWire), (4, QWire)]

make_gate_circ :: [Quip.Gate] -> GateCirc
make_gate_circ gates = GateCirc { inputs  = iomap_4
                                , gates   = gates
                                , outputs = iomap_4
                                , size    = 10
                                }

getOp :: Int -> Qasm.Operand
getOp n = Qasm.Cell "input_qwires" n

mod0 = Qasm.nullGateMod
mod2 = Qasm.addCtrlsToMod 2 $ mod0
mod3 = Qasm.addCtrlsToMod 1 $ mod0

gate1 = Quip.NamedGate Quip.GateX False [4] $ map Quip.Pos [2, 3, 0, 1]
gate2 = Quip.PhaseGate 1.0 [Quip.Pos 0, Quip.Pos 1, Quip.Pos 2]
gate3 = Quip.NamedGate Quip.GateS False [2] []

pi1 = Times Pi (DecFloat "1.0")

qgate1 = Qasm.NamedGate Qasm.GateCCX [] [getOp 0, getOp 1, getOp 2, getOp 3, getOp 4] mod2
qgate2 = Qasm.NamedGate Qasm.GateCP [pi1] [getOp 2, getOp 0, getOp 1] mod3
qgate3 = Qasm.NamedGate Qasm.GateS [] [getOp 2] mod0

dstmt1 = AstQubitDecl (Just 5) "input_qwires"

gstmt1 = AstGateStmt 1 qgate1
gstmt2 = AstGateStmt 1 qgate2
gstmt3 = AstGateStmt 1 qgate3

test7 = TestCase (assertEqual "Can convert CCX."
                              [dstmt1, gstmt1]
                              (translate $ make_gate_circ [gate1]))

test8 = TestCase (assertEqual "Can convert GPhase gates."
                              [dstmt1, gstmt2]
                              (translate $ make_gate_circ [gate2]))

test9 = TestCase (assertEqual "Can convert mixed unitary gates."
                              [dstmt1, gstmt1, gstmt2, gstmt3]
                              (translate $ make_gate_circ [gate1, gate2, gate3]))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "Empty_Circ_0_to_0" test1,
                                     TestLabel "Empty_Circ_1_to_0" test2,
                                     TestLabel "Empty_Circ_1_to_1" test3,
                                     TestLabel "Empty_Circ_3_to_0" test4,
                                     TestLabel "Empty_Circ_3_to_1" test5,
                                     TestLabel "Empty_Circ_3_to_3" test6,
                                     TestLabel "CCX" test7,
                                     TestLabel "GPhase" test8,
                                     TestLabel "MixedUnitary" test9]

main = defaultMain tests
