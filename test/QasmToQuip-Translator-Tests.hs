module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.AST
import LinguaQuanta.Qasm.Gate as Qasm
import LinguaQuanta.Qasm.GateName as Qasm
import LinguaQuanta.Qasm.GateName
import LinguaQuanta.Qasm.Language
import LinguaQuanta.QasmToQuip.Translator
import LinguaQuanta.Quip.Gate as Quip
import LinguaQuanta.Quip.GateName as Quip
import LinguaQuanta.Quip.Quipper
import LinguaQuanta.Quip.Wire

import qualified Data.IntMap.Strict as IntMap

-----------------------------------------------------------------------------------------
-- Basic QWire Tests.

dstmt1 = AstQubitDecl Nothing  "decl1"
dstmt2 = AstQubitDecl (Just 4) "decl1"
dstmt3 = AstQubitDecl (Nothing) "decl2"

circ1 = translate []
circ2 = translate [dstmt1]
circ3 = translate [dstmt2]
circ4 = translate [dstmt2, dstmt3]

test1 = TestCase (assertEqual "translate (inputs): empty circuit."
                              IntMap.empty
                              (outputs circ1))

test2 = TestCase (assertEqual "translate (gates): empty circuit."
                              []
                              (gates circ1))

test3 = TestCase (assertEqual "translate (outputs): empty circuit."
                              IntMap.empty
                              (outputs circ1))

test4 = TestCase (assertEqual "translate (size): empty circuit."
                              0
                              (size circ1))

circ2_io = IntMap.fromList [(0, QWire)]

test5 = TestCase (assertEqual "translate (inputs): single qbit."
                              circ2_io
                              (outputs circ2))

test6 = TestCase (assertEqual "translate (gates): single qbit."
                              []
                              (gates circ2))

test7 = TestCase (assertEqual "translate (outputs): single qbit."
                              circ2_io
                              (outputs circ2))

test8 = TestCase (assertEqual "translate (size): single qbit."
                              1
                              (size circ2))

circ3_io = IntMap.fromList [(0, QWire), (1, QWire), (2, QWire), (3, QWire)]

test9 = TestCase (assertEqual "translate (inputs): qbit array."
                              circ3_io
                              (outputs circ3))

test10 = TestCase (assertEqual "translate (gates): qbit array."
                               []
                               (gates circ3))

test11 = TestCase (assertEqual "translate (outputs): qbit array."
                               circ3_io
                               (outputs circ3))

test12 = TestCase (assertEqual "translate (size): qbit array."
                               4
                               (size circ3))

circ4_io = IntMap.fromList [(0, QWire), (1, QWire), (2, QWire), (3, QWire), (4, QWire)]

test13 = TestCase (assertEqual "translate (inputs): qbit array and single qubit."
                               circ4_io
                               (outputs circ4))

test14 = TestCase (assertEqual "translate (gates): qbit array and single qubit."
                               []
                               (gates circ4))

test15 = TestCase (assertEqual "translate (outputs): qbit array and single qubit."
                               circ4_io
                               (outputs circ4))

test16 = TestCase (assertEqual "translate (size): qbit array and single qubit."
                               5
                               (size circ4))

-----------------------------------------------------------------------------------------
-- Translating Unitaries

getOp :: Int -> Qasm.Operand
getOp 4 = QRef "decl2"
getOp n = Cell "decl1" n

mod0 = Qasm.nullGateMod
mod1 = Qasm.negateMod mod0
mod2 = Qasm.addCtrlsToMod 2 $ mod1
mod3 = Qasm.addNegCtrlsToMod 1 $ addCtrlsToMod 2 $ mod0

gate1 = Qasm.NamedGate Qasm.GateCCX [] [getOp 0, getOp 1, getOp 2, getOp 3, getOp 4] mod2
gate2 = Qasm.NamedGate Qasm.GateSdg [] [getOp 2] mod1
gate3 = Qasm.GPhaseGate Pi [getOp 0, getOp 1, getOp 2] mod3

gstmt1 = AstGateStmt 0 gate1
gstmt2 = AstGateStmt 1 gate3
gstmt3 = AstGateStmt 2 gate2
gstmt4 = AstGateStmt 3 gate1

qgate1 = Quip.NamedGate Quip.GateX False [4] $ map Quip.Pos [2, 3, 0, 1]
qgate2 = Quip.PhaseGate 1.0 [Quip.Neg 0, Quip.Pos 1, Quip.Pos 2]
qgate3 = Quip.NamedGate Quip.GateS False [2] []

circ5 = translate [dstmt2, dstmt3, gstmt1]
circ6 = translate [dstmt2, gstmt2]
circ7 = translate [dstmt2, dstmt3, gstmt3, gstmt2, gstmt4]

test17 = TestCase (assertEqual "translate (inputs): isolated CCX gate."
                               circ4_io
                               (outputs circ5))

test18 = TestCase (assertEqual "translate (gates): isolated named gate."
                               [qgate1]
                               (gates circ5))

test19 = TestCase (assertEqual "translate (outputs): isolated named gate."
                               circ4_io
                               (outputs circ5))

test20 = TestCase (assertEqual "translate (size): isolated named gate."
                               5
                               (size circ5))

test21 = TestCase (assertEqual "translate (inputs): isolated gphase gate."
                              circ3_io
                              (outputs circ6))

test22 = TestCase (assertEqual "translate (gates): isolated gphase gate."
                               [qgate2]
                               (gates circ6))

test23 = TestCase (assertEqual "translate (outputs): isolated gphase gate."
                               circ3_io
                               (outputs circ6))

test24 = TestCase (assertEqual "translate (size): isolated gphase gate."
                               4
                               (size circ6))

test25 = TestCase (assertEqual "translate (inputs): mixed unitary gates."
                               circ4_io
                               (outputs circ7))

test26 = TestCase (assertEqual "translate (gates): mixed unitary gates."
                               [qgate3, qgate3, qgate2, qgate1, qgate1, qgate1]
                               (gates circ7))

test27 = TestCase (assertEqual "translate (outputs): mixed unitary gates."
                               circ4_io
                               (outputs circ7))

test28 = TestCase (assertEqual "translate (size): mixed unitary gates."
                               5
                               (size circ7))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "Empty_Inputs" test1,
                                     TestLabel "Empty_Gates" test2,
                                     TestLabel "Empty_Outputs" test3,
                                     TestLabel "Empty_Size" test4,
                                     TestLabel "1Qbit_Inputs" test5,
                                     TestLabel "1Qbit_Gates" test6,
                                     TestLabel "1Qbit_Outputs" test7,
                                     TestLabel "1Qbit_Size" test8,
                                     TestLabel "1Qarr_Inputs" test9,
                                     TestLabel "1Qarr_Gates" test10,
                                     TestLabel "1Qarr_Outputs" test11,
                                     TestLabel "1Qarr_Size" test12,
                                     TestLabel "1Qarr1Qbit_Inputs" test13,
                                     TestLabel "1Qarr1Qbit_Gates" test14,
                                     TestLabel "1Qarr1Qbit_Outputs" test15,
                                     TestLabel "1Qarr1Qbit_Size" test16,
                                     TestLabel "CCX_Inputs" test17,
                                     TestLabel "CCX_Gates" test18,
                                     TestLabel "CCX_Outputs" test19,
                                     TestLabel "CCX_Size" test20,
                                     TestLabel "GPhase_Inputs" test21,
                                     TestLabel "GPhase_Gates" test22,
                                     TestLabel "GPhase_Outputs" test23,
                                     TestLabel "GPhase_Size" test24,
                                     TestLabel "MixedUnitary_Inputs" test25,
                                     TestLabel "MixedUnitary_Gates" test26,
                                     TestLabel "MixedUnitary_Outputs" test27,
                                     TestLabel "MixedUnitary_Size" test28]

main = defaultMain tests
