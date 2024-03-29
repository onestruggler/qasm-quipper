module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.AST
import LinguaQuanta.Qasm.Gate as Qasm
import LinguaQuanta.Qasm.GateName as Qasm
import LinguaQuanta.Qasm.Language
import LinguaQuanta.Qasm.Operand
import LinguaQuanta.Quip.Gate as Quip
import LinguaQuanta.Quip.GateName as Quip
import LinguaQuanta.Quip.Quipper
import LinguaQuanta.QuipToQasm.Translator

import qualified Data.IntMap.Strict as IntMap

-----------------------------------------------------------------------------------------
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
gate3 = Quip.RotGate Quip.RotExpZ False 2.0 [0] []
gate4 = Quip.NamedGate Quip.GateS False [2] []

pi1  = Times Pi (DecFloat "1.0")
four = Times (DecFloat "2.0") (DecInt "2")

qgate1 = Qasm.NamedGate Qasm.GateCCX [] [getOp 0, getOp 1, getOp 2, getOp 3, getOp 4] mod2
qgate2 = Qasm.NamedGate Qasm.GateCP [pi1] [getOp 2, getOp 0, getOp 1] mod3
qgate3 = Qasm.NamedGate Qasm.GateRZ [four] [getOp 0] mod0
qgate4 = Qasm.NamedGate Qasm.GateS [] [getOp 2] mod0

dstmt1 = AstQubitDecl (Just 5) "input_qwires"

gstmt1 = AstGateStmt 0 qgate1
gstmt2 = AstGateStmt 0 qgate2
gstmt3 = AstGateStmt 0 qgate3
gstmt4 = AstGateStmt 0 qgate4

test7 = TestCase (assertEqual "Can convert CCX."
                              [dstmt1, gstmt1]
                              (translate $ make_gate_circ [gate1]))

test8 = TestCase (assertEqual "Can convert GPhase gates."
                              [dstmt1, gstmt2]
                              (translate $ make_gate_circ [gate2]))

test9 = TestCase (assertEqual "Can convert CZ."
                              [dstmt1, gstmt3]
                              (translate $ make_gate_circ [gate3]))

test10 = TestCase (assertEqual "Can convert mixed unitary gates."
                              [dstmt1, gstmt1, gstmt2, gstmt3, gstmt4]
                              (translate $ make_gate_circ [gate1, gate2, gate3, gate4]))

-----------------------------------------------------------------------------------------
-- * Classical Wire Tests

iomap_5 = IntMap.fromList[(1, QWire), (3, CWire)]
iomap_6 = IntMap.fromList[(1, QWire), (3, CWire), (5, QWire), (7, CWire), (9, QWire)]

test11 = TestCase (assertEqual "Can convert a 1-to-0 bit circuit (with qubits)."
                               [AstQubitDecl (Just 1) "input_qwires",
                                AstBitDecl (Just 1) "input_cwires"]
                               (translate $ make_wire_circ iomap_5 iomap_1))

test12 = TestCase (assertEqual "Can convert a 1-to-1 bit circuit (with qubits)."
                               [AstQubitDecl (Just 1) "input_qwires",
                                AstBitDecl (Just 1) "input_cwires"]
                               (translate $ make_wire_circ iomap_5 iomap_5))

test13 = TestCase (assertEqual "Can convert a 2-to-0 bit circuit (with qubits)."
                               [AstQubitDecl (Just 3) "input_qwires",
                                AstBitDecl (Just 2) "input_cwires"]
                               (translate $ make_wire_circ iomap_6 iomap_1))

test14 = TestCase (assertEqual "Can convert a 2-to-1 bit circuit (with qubits)."
                               [AstQubitDecl (Just 3) "input_qwires",
                                AstBitDecl (Just 2) "input_cwires"]
                               (translate $ make_wire_circ iomap_6 iomap_5))

test15 = TestCase (assertEqual "Can convert a 2-to-2 bit circuit (with qubits)."
                               [AstQubitDecl (Just 3) "input_qwires",
                                AstBitDecl (Just 2) "input_cwires"]
                               (translate $ make_wire_circ iomap_6 iomap_6))

-----------------------------------------------------------------------------------------
-- * QMeas translations.

test16 = TestCase (assertEqual "Can translate a sequence of QMeas gates (no prep)."
                               [AstQubitDecl (Just 5) "input_qwires",
                                AstBitDecl Nothing "shadow_cwire_0",
                                AstAssign "shadow_cwire_0" Nothing $ QuipMeasure target1,
                                AstBitDecl Nothing "shadow_cwire_1",
                                AstAssign "shadow_cwire_1" Nothing $ QuipMeasure target2]
                               (translate $ make_gate_circ [QMeasGate 1, QMeasGate 3]))
    where target1 = Cell "input_qwires" 1
          target2 = Cell "input_qwires" 3

-----------------------------------------------------------------------------------------
-- * Ancilla translations.

iomap_7 = IntMap.fromList [(0, QWire), (1, CWire), (2, QWire), (3, CWire)]
iomap_8 = IntMap.fromList [(1, CWire), (2, QWire)]

test17 = TestCase (assertEqual "Can translate a sequence of init and term gates."
                               [AstQubitDecl (Just 2) "input_qwires",
                                AstBitDecl (Just 2) "input_cwires",
                                AstQubitDecl Nothing "shadow_qwire_0",
                                AstCall $ QuipQInit1 $ QRef "shadow_qwire_0",
                                AstCall $ QuipQTerm1 $ QRef "shadow_qwire_0",
                                AstCall $ QuipQDiscard $ Cell "input_qwires" 0,
                                AstCall $ QuipQInit0 $ QRef "shadow_qwire_0",
                                AstCall $ QuipQTerm0 $ QRef "shadow_qwire_0",
                                AstBitDecl Nothing "shadow_cwire_1",
                                AstAssign "shadow_cwire_1" Nothing QuipCInit1,
                                AstAssign "shadow_cwire_1" Nothing QuipCTerm1,
                                AstAssign "input_cwires" (Just 1) QuipCDiscard,
                                AstAssign "shadow_cwire_1" Nothing QuipCInit0,
                                AstAssign "shadow_cwire_1" Nothing QuipCTerm0]
                               (translate qcirc))
    where gates = [QInitGate True 5, QTermGate True 5, QDiscardGate 0,
                   QInitGate False 5, QTermGate False 5,
                   CInitGate True 5, CTermGate True 5, CDiscardGate 3,
                   CInitGate False 5, CTermGate False 5]
          qcirc = GateCirc { inputs  = iomap_7
                           , gates   = gates
                           , outputs = iomap_8
                           , size    = 5
                           }

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
                                     TestLabel "CZ" test9,
                                     TestLabel "MixedUnitary" test10,
                                     TestLabel "Empty_Bit_Circ_1_to_0" test11,
                                     TestLabel "Empty_Bit_Circ_1_to_1" test12,
                                     TestLabel "Empty_Bit_Circ_2_to_0" test13,
                                     TestLabel "Empty_Bit_Circ_2_to_1" test14,
                                     TestLabel "Empty_Bit_Circ_2_to_2" test15,
                                     TestLabel "QMeas_Without_Prep" test16,
                                     TestLabel "Ancilla" test17]

main = defaultMain tests
