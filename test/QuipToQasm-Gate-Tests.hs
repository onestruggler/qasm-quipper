module Main where

import qualified Data.IntMap.Strict as IntMap
import Data.Maybe
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.AST (AstStmt(..))
import LinguaQuanta.Qasm.Gate as Qasm
import qualified LinguaQuanta.Qasm.GateName as Qasm
import LinguaQuanta.Qasm.Language
import LinguaQuanta.Quip.Gate as Quip
import qualified LinguaQuanta.Quip.GateName as Quip
import LinguaQuanta.Quip.Wire
import LinguaQuanta.QuipToQasm.Gate
import LinguaQuanta.QuipToQasm.Wire

-----------------------------------------------------------------------------------------
-- Wires and allocations.

qwires = [(1, QWire), (2, QWire), (3, QWire), (4, QWire), (5, QWire), (6, QWire)]
qalloc = allocateInputWires $ IntMap.fromList qwires

-----------------------------------------------------------------------------------------
-- Translating the Pauli-X gate without controls (multiple configurations).

test1 = TestCase (assertEqual "namedGateTransl: support for uncontrolled Pauli-X (1/4)."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateX False [1] []))
    where decl = QReg "input_qwires" (DecInt "0")
          gate = Qasm.NamedGate Qasm.GateX [] [decl] nullGateMod

test2 = TestCase (assertEqual "namedGateTransl: support for uncontrolled Pauli-X (2/4)."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateX False [3] []))
    where decl = QReg "input_qwires" (DecInt "2")
          gate = Qasm.NamedGate Qasm.GateX [] [decl] nullGateMod

test3 = TestCase (assertEqual "namedGateTransl: support for uncontrolled Pauli-X (3/4)."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateX True [1] []))
    where decl = QReg "input_qwires" (DecInt "0")
          gate = Qasm.NamedGate Qasm.GateX [] [decl] nullGateMod

test4 = TestCase (assertEqual "namedGateTransl: support for uncontrolled Pauli-X (4/4)."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateX True [3] []))
    where decl = QReg "input_qwires" (DecInt "2")
          gate = Qasm.NamedGate Qasm.GateX [] [decl] nullGateMod

-----------------------------------------------------------------------------------------
-- Translating uncontrolled gates with named controlled instances (single configuration).

test5 = TestCase (assertEqual "namedGateTransl: support for uncontrolled Pauli-Y."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateY False [1] []))
    where decl = QReg "input_qwires" (DecInt "0")
          gate = Qasm.NamedGate Qasm.GateY [] [decl] nullGateMod

test6 = TestCase (assertEqual "namedGateTransl: support for uncontrolled Pauli-Z."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateZ False [1] []))
    where decl = QReg "input_qwires" (DecInt "0")
          gate = Qasm.NamedGate Qasm.GateZ [] [decl] nullGateMod

test7 = TestCase (assertEqual "namedGateTransl: support for uncontrolled H gate."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateH False [1] []))
    where decl = QReg "input_qwires" (DecInt "0")
          gate = Qasm.NamedGate Qasm.GateH [] [decl] nullGateMod

test8 = TestCase (assertEqual "namedGateTransl: support for uncontrolled swap gate."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateSwap False [1] []))
    where decl = QReg "input_qwires" (DecInt "0")
          gate = Qasm.NamedGate Qasm.GateSwap [] [decl] nullGateMod

-----------------------------------------------------------------------------------------
-- Translating the Pauli-Y gate with a single control (multiple configurations).

test9 = TestCase (assertEqual "namedGateTransl: support for controlled Pauli-Y (1/5)."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateY False [1] ctrls))
    where ctrls = [Quip.Pos 2]
          decl1 = QReg "input_qwires" (DecInt "1")
          decl2 = QReg "input_qwires" (DecInt "0")
          gate  = Qasm.NamedGate Qasm.GateCY [] [decl1, decl2] nullGateMod

test10 = TestCase (assertEqual "namedGateTransl: support for controlled Pauli-Y (2/5)."
                               [AstGateStmt 1 neg, AstGateStmt 1 gate, AstGateStmt 1 neg]
                               (namedGateTransl qalloc Quip.GateY True [1] ctrls))
    where ctrls = [Quip.Neg 2]
          decl1 = QReg "input_qwires" (DecInt "1")
          decl2 = QReg "input_qwires" (DecInt "0")
          neg   = Qasm.NamedGate Qasm.GateX [] [decl1] nullGateMod
          gate  = Qasm.NamedGate Qasm.GateCY [] [decl1, decl2] nullGateMod

test11 = TestCase (assertEqual "namedGateTransl: support for controlled Pauli-Y (3/5)."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateY True [1] ctrls))
    where ctrls = [Quip.Pos 2]
          decl1 = QReg "input_qwires" (DecInt "1")
          decl2 = QReg "input_qwires" (DecInt "0")
          gate  = Qasm.NamedGate Qasm.GateCY [] [decl1, decl2] nullGateMod

test12 = TestCase (assertEqual "namedGateTransl: support for controlled Pauli-Y (4/5)."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateY False [3] ctrls))
    where ctrls = [Quip.Pos 4]
          decl1 = QReg "input_qwires" (DecInt "3")
          decl2 = QReg "input_qwires" (DecInt "2")
          gate  = Qasm.NamedGate Qasm.GateCY [] [decl1, decl2] nullGateMod

test13 = TestCase (assertEqual "namedGateTransl: support for controlled Pauli-Y (5/5)."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateY True [3] ctrls))
    where ctrls = [Quip.Pos 4]
          decl1 = QReg "input_qwires" (DecInt "3")
          decl2 = QReg "input_qwires" (DecInt "2")
          gate  = Qasm.NamedGate Qasm.GateCY [] [decl1, decl2] nullGateMod

-----------------------------------------------------------------------------------------
-- Translating the Pauli-Y gate with many controls (single configurations).

test14 = TestCase (assertEqual "namedGateTransl: support for 2-controlled Pauli-Y."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateY False [1] ctrls))
    where ctrls = [Quip.Pos 2, Quip.Pos 3]
          decl1 = QReg "input_qwires" (DecInt "2")
          decl2 = QReg "input_qwires" (DecInt "1")
          decl3 = QReg "input_qwires" (DecInt "0")
          mod   = addCtrlsToMod 1 nullGateMod
          gate  = Qasm.NamedGate Qasm.GateCY [] [decl1, decl2, decl3] mod

test15 = TestCase (assertEqual "namedGateTransl: support for 3-controlled Pauli-Y."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateY False [1] ctrls))
    where ctrls = [Quip.Pos 2, Quip.Pos 3, Quip.Neg 4]
          decl1 = QReg "input_qwires" (DecInt "2")
          decl2 = QReg "input_qwires" (DecInt "3")
          decl3 = QReg "input_qwires" (DecInt "1")
          decl4 = QReg "input_qwires" (DecInt "0")
          ops   = [decl1, decl2, decl3, decl4]
          mod   = addCtrlsToMod 1 $ addNegCtrlsToMod 1 $ nullGateMod
          gate  = Qasm.NamedGate Qasm.GateCY [] ops mod

test16 = TestCase (assertEqual "namedGateTransl: support for 4-controlled Pauli-Y."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateY False [1] ctrls))
    where ctrls = [Quip.Pos 2, Quip.Pos 3, Quip.Neg 4, Quip.Neg 5]
          decl1 = QReg "input_qwires" (DecInt "2")
          decl2 = QReg "input_qwires" (DecInt "3")
          decl3 = QReg "input_qwires" (DecInt "4")
          decl4 = QReg "input_qwires" (DecInt "1")
          decl5 = QReg "input_qwires" (DecInt "0")
          ops   = [decl1, decl2, decl3, decl4, decl5]
          mod   = addCtrlsToMod 1 $ addNegCtrlsToMod 1 $ addNegCtrlsToMod 1 $ nullGateMod
          gate  = Qasm.NamedGate Qasm.GateCY [] ops mod

-----------------------------------------------------------------------------------------
-- Translating controlled gates with named controlled instances (single configuration).

test17 = TestCase (assertEqual "namedGateTransl: support for singly controlled Pauli-X."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateX False [1] ctrls))
    where ctrls = [Quip.Pos 2]
          decl1 = QReg "input_qwires" (DecInt "1")
          decl2 = QReg "input_qwires" (DecInt "0")
          gate  = Qasm.NamedGate Qasm.GateCX [] [decl1, decl2] nullGateMod

test18 = TestCase (assertEqual "namedGateTransl: support for singly controlled Pauli-Z."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateZ False [1] ctrls))
    where ctrls = [Quip.Pos 2]
          decl1 = QReg "input_qwires" (DecInt "1")
          decl2 = QReg "input_qwires" (DecInt "0")
          gate  = Qasm.NamedGate Qasm.GateCZ [] [decl1, decl2] nullGateMod

test19 = TestCase (assertEqual "namedGateTransl: support for singly controlled H gate."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateH False [1] ctrls))
    where ctrls = [Quip.Pos 2]
          decl1 = QReg "input_qwires" (DecInt "1")
          decl2 = QReg "input_qwires" (DecInt "0")
          gate  = Qasm.NamedGate Qasm.GateCH [] [decl1, decl2] nullGateMod

test20 = TestCase (assertEqual "namedGateTransl: support for singly controlled swap gate."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateSwap False [1] ctrls))
    where ctrls = [Quip.Pos 2]
          decl1 = QReg "input_qwires" (DecInt "1")
          decl2 = QReg "input_qwires" (DecInt "0")
          gate  = Qasm.NamedGate Qasm.GateCSwap [] [decl1, decl2] nullGateMod

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "GateX_NoCtrl_NoInv_Q1" test1,
                                     TestLabel "GateX_NoCtrl_NoInv_Q3" test2,
                                     TestLabel "GateX_NoCtrl_Inv_Q1" test3,
                                     TestLabel "GateX_NoCtrl_Inv_Q3" test4,
                                     TestLabel "GateY" test5,
                                     TestLabel "GateZ" test6,
                                     TestLabel "GateH" test7,
                                     TestLabel "GateSwap" test8,
                                     TestLabel "GateY_Pos2_NoInv_Q1" test9,
                                     TestLabel "GateY_Neg2_NoInv_Q1" test10,
                                     TestLabel "GateY_Pos2_Inv_Q1" test11,
                                     TestLabel "GateY_Pos4_NoInv_Q3" test12,
                                     TestLabel "GateY_Pos4_Inv_Q3" test13,
                                     TestLabel "GateY_Pos2_Pos3" test14,
                                     TestLabel "GateY_Pos2_Pos3_Neg4" test15,
                                     TestLabel "GateY_Pos2_Pos3_Neg4_Neg5" test16,
                                     TestLabel "GateCX" test17,
                                     TestLabel "GateCZ" test18,
                                     TestLabel "GateCH" test19,
                                     TestLabel "GateCSwap" test20]

main = defaultMain tests
