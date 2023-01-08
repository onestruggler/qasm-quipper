module Main where

import qualified Data.IntMap.Strict as IntMap
import Data.Maybe
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.AST (AstStmt(..))
import LinguaQuanta.Qasm.Gate
import qualified LinguaQuanta.Qasm.GateName as Qasm
import LinguaQuanta.Qasm.Language
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

--[xTranslQ2]    = namedGateTransl alloc Quip.GateX False [2]
--[xTranslQ3]    = namedGateTransl alloc Quip.GateX False [3]
--[xTranslInvQ1] = namedGateTransl alloc Quip.GateX False [3]

test1 = TestCase (assertEqual "namedGateTransl: support for uncontrolled Pauli-X (1/4)."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateX False [1] []))
    where decl = QReg "input_qwires" (DecInt "0")
          gate = NamedGate Qasm.GateX [] [decl] nullGateMod

test2 = TestCase (assertEqual "namedGateTransl: support for uncontrolled Pauli-X (2/4)."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateX False [3] []))
    where decl = QReg "input_qwires" (DecInt "2")
          gate = NamedGate Qasm.GateX [] [decl] nullGateMod

test3 = TestCase (assertEqual "namedGateTransl: support for uncontrolled Pauli-X (3/4)."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateX True [1] []))
    where decl = QReg "input_qwires" (DecInt "0")
          gate = NamedGate Qasm.GateX [] [decl] nullGateMod

test4 = TestCase (assertEqual "namedGateTransl: support for uncontrolled Pauli-X (4/4)."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateX True [3] []))
    where decl = QReg "input_qwires" (DecInt "2")
          gate = NamedGate Qasm.GateX [] [decl] nullGateMod

-----------------------------------------------------------------------------------------
-- Translating uncontrolled gates with named controlled instances (single configuration).

test5 = TestCase (assertEqual "namedGateTransl: support for uncontrolled Pauli-Y."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateY False [1] []))
    where decl = QReg "input_qwires" (DecInt "0")
          gate = NamedGate Qasm.GateY [] [decl] nullGateMod

test6 = TestCase (assertEqual "namedGateTransl: support for uncontrolled Pauli-Z."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateZ False [1] []))
    where decl = QReg "input_qwires" (DecInt "0")
          gate = NamedGate Qasm.GateZ [] [decl] nullGateMod

test7 = TestCase (assertEqual "namedGateTransl: support for uncontrolled H gate."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateH False [1] []))
    where decl = QReg "input_qwires" (DecInt "0")
          gate = NamedGate Qasm.GateH [] [decl] nullGateMod

test8 = TestCase (assertEqual "namedGateTransl: support for uncontrolled swap gate."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateSwap False [1] []))
    where decl = QReg "input_qwires" (DecInt "0")
          gate = NamedGate Qasm.GateSwap [] [decl] nullGateMod

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "GateX_NoCtrl_NoInv_Q1" test1,
                                     TestLabel "GateX_NoCtrl_NoInv_Q1" test2,
                                     TestLabel "GateX_NoCtrl_NoInv_Q1" test3,
                                     TestLabel "GateX_NoCtrl_NoInv_Q1" test4,
                                     TestLabel "GateY" test5,
                                     TestLabel "GateZ" test6,
                                     TestLabel "GateH" test7,
                                     TestLabel "GateSwap" test8]

main = defaultMain tests
