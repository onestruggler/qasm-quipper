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
    where decl = Cell "input_qwires" 0
          gate = Qasm.NamedGate Qasm.GateX [] [decl] nullGateMod

test2 = TestCase (assertEqual "namedGateTransl: support for uncontrolled Pauli-X (2/4)."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateX False [3] []))
    where decl = Cell "input_qwires" 2
          gate = Qasm.NamedGate Qasm.GateX [] [decl] nullGateMod

test3 = TestCase (assertEqual "namedGateTransl: support for uncontrolled Pauli-X (3/4)."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateX True [1] []))
    where decl = Cell "input_qwires" 0
          gate = Qasm.NamedGate Qasm.GateX [] [decl] nullGateMod

test4 = TestCase (assertEqual "namedGateTransl: support for uncontrolled Pauli-X (4/4)."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateX True [3] []))
    where decl = Cell "input_qwires" 2
          gate = Qasm.NamedGate Qasm.GateX [] [decl] nullGateMod

-----------------------------------------------------------------------------------------
-- Translating uncontrolled gates with named controlled instances (single configuration).

test5 = TestCase (assertEqual "namedGateTransl: support for uncontrolled Pauli-Y."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateY False [1] []))
    where decl = Cell "input_qwires" 0
          gate = Qasm.NamedGate Qasm.GateY [] [decl] nullGateMod

test6 = TestCase (assertEqual "namedGateTransl: support for uncontrolled Pauli-Z."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateZ False [1] []))
    where decl = Cell "input_qwires" 0
          gate = Qasm.NamedGate Qasm.GateZ [] [decl] nullGateMod

test7 = TestCase (assertEqual "namedGateTransl: support for uncontrolled H gate."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateH False [1] []))
    where decl = Cell "input_qwires" 0
          gate = Qasm.NamedGate Qasm.GateH [] [decl] nullGateMod

test8 = TestCase (assertEqual "namedGateTransl: support for uncontrolled swap gate."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateSwap False [1, 2] []))
    where decl1 = Cell "input_qwires" 0
          decl2 = Cell "input_qwires" 1
          gate = Qasm.NamedGate Qasm.GateSwap [] [decl1, decl2] nullGateMod

-----------------------------------------------------------------------------------------
-- Translating the Pauli-Y gate with a single control (multiple configurations).

test9 = TestCase (assertEqual "namedGateTransl: support for controlled Pauli-Y (1/5)."
                              [AstGateStmt 1 gate]
                              (namedGateTransl qalloc Quip.GateY False [1] ctrls))
    where ctrls = [Quip.Pos 2]
          decl1 = Cell "input_qwires" 1
          decl2 = Cell "input_qwires" 0
          gate  = Qasm.NamedGate Qasm.GateCY [] [decl1, decl2] nullGateMod

test10 = TestCase (assertEqual "namedGateTransl: support for controlled Pauli-Y (2/5)."
                               [AstGateStmt 1 neg, AstGateStmt 1 gate, AstGateStmt 1 neg]
                               (namedGateTransl qalloc Quip.GateY True [1] ctrls))
    where ctrls = [Quip.Neg 2]
          decl1 = Cell "input_qwires" 1
          decl2 = Cell "input_qwires" 0
          neg   = Qasm.NamedGate Qasm.GateX [] [decl1] nullGateMod
          gate  = Qasm.NamedGate Qasm.GateCY [] [decl1, decl2] nullGateMod

test11 = TestCase (assertEqual "namedGateTransl: support for controlled Pauli-Y (3/5)."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateY True [1] ctrls))
    where ctrls = [Quip.Pos 2]
          decl1 = Cell "input_qwires" 1
          decl2 = Cell "input_qwires" 0
          gate  = Qasm.NamedGate Qasm.GateCY [] [decl1, decl2] nullGateMod

test12 = TestCase (assertEqual "namedGateTransl: support for controlled Pauli-Y (4/5)."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateY False [3] ctrls))
    where ctrls = [Quip.Pos 4]
          decl1 = Cell "input_qwires" 3
          decl2 = Cell "input_qwires" 2
          gate  = Qasm.NamedGate Qasm.GateCY [] [decl1, decl2] nullGateMod

test13 = TestCase (assertEqual "namedGateTransl: support for controlled Pauli-Y (5/5)."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateY True [3] ctrls))
    where ctrls = [Quip.Pos 4]
          decl1 = Cell "input_qwires" 3
          decl2 = Cell "input_qwires" 2
          gate  = Qasm.NamedGate Qasm.GateCY [] [decl1, decl2] nullGateMod

-----------------------------------------------------------------------------------------
-- Translating the Pauli-Y gate with many controls (single configurations).

test14 = TestCase (assertEqual "namedGateTransl: support for 2-controlled Pauli-Y."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateY False [1] ctrls))
    where ctrls = [Quip.Pos 2, Quip.Pos 3]
          decl1 = Cell "input_qwires" 2
          decl2 = Cell "input_qwires" 1
          decl3 = Cell "input_qwires" 0
          mod   = addCtrlsToMod 1 nullGateMod
          gate  = Qasm.NamedGate Qasm.GateCY [] [decl1, decl2, decl3] mod

test15 = TestCase (assertEqual "namedGateTransl: support for 3-controlled Pauli-Y."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateY False [1] ctrls))
    where ctrls = [Quip.Pos 2, Quip.Pos 3, Quip.Neg 4]
          decl1 = Cell "input_qwires" 2
          decl2 = Cell "input_qwires" 3
          decl3 = Cell "input_qwires" 1
          decl4 = Cell "input_qwires" 0
          ops   = [decl1, decl2, decl3, decl4]
          mod   = addCtrlsToMod 1 $ addNegCtrlsToMod 1 $ nullGateMod
          gate  = Qasm.NamedGate Qasm.GateCY [] ops mod

test16 = TestCase (assertEqual "namedGateTransl: support for 4-controlled Pauli-Y."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateY False [1] ctrls))
    where ctrls = [Quip.Pos 2, Quip.Pos 3, Quip.Neg 4, Quip.Neg 5]
          decl1 = Cell "input_qwires" 2
          decl2 = Cell "input_qwires" 3
          decl3 = Cell "input_qwires" 4
          decl4 = Cell "input_qwires" 1
          decl5 = Cell "input_qwires" 0
          ops   = [decl1, decl2, decl3, decl4, decl5]
          mod   = addCtrlsToMod 1 $ addNegCtrlsToMod 1 $ addNegCtrlsToMod 1 $ nullGateMod
          gate  = Qasm.NamedGate Qasm.GateCY [] ops mod

-----------------------------------------------------------------------------------------
-- Translating controlled gates with named controlled instances (single configuration).

test17 = TestCase (assertEqual "namedGateTransl: support for singly controlled Pauli-X."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateX False [1] ctrls))
    where ctrls = [Quip.Pos 2]
          decl1 = Cell "input_qwires" 1
          decl2 = Cell "input_qwires" 0
          gate  = Qasm.NamedGate Qasm.GateCX [] [decl1, decl2] nullGateMod

test18 = TestCase (assertEqual "namedGateTransl: support for singly controlled Pauli-Z."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateZ False [1] ctrls))
    where ctrls = [Quip.Pos 2]
          decl1 = Cell "input_qwires" 1
          decl2 = Cell "input_qwires" 0
          gate  = Qasm.NamedGate Qasm.GateCZ [] [decl1, decl2] nullGateMod

test19 = TestCase (assertEqual "namedGateTransl: support for singly controlled H gate."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateH False [1] ctrls))
    where ctrls = [Quip.Pos 2]
          decl1 = Cell "input_qwires" 1
          decl2 = Cell "input_qwires" 0
          gate  = Qasm.NamedGate Qasm.GateCH [] [decl1, decl2] nullGateMod

test20 = TestCase (assertEqual "namedGateTransl: support for singly controlled swap gate."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateSwap False [1, 2] ctrls))
    where ctrls = [Quip.Pos 3]
          decl1 = Cell "input_qwires" 2
          decl2 = Cell "input_qwires" 0
          decl3 = Cell "input_qwires" 1
          gate  = Qasm.NamedGate Qasm.GateCSwap [] [decl1, decl2, decl3] nullGateMod

-----------------------------------------------------------------------------------------
-- Translating the V gate (multiple configurations).

test21 = TestCase (assertEqual "namedGateTransl: support for uncontrolled V gate (1/4)."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateV False [1] []))
    where decl = Cell "input_qwires" 0
          gate = Qasm.NamedGate Qasm.GateSX [] [decl] nullGateMod

test22 = TestCase (assertEqual "namedGateTransl: support for uncontrolled V gate (2/4)."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateV True [1] []))
    where decl = Cell "input_qwires" 0
          mod  = negateMod nullGateMod
          gate = Qasm.NamedGate Qasm.GateSX [] [decl] mod

test23 = TestCase (assertEqual "namedGateTransl: support for uncontrolled V gate (3/4)."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateV False [3] []))
    where decl = Cell "input_qwires" 2
          gate = Qasm.NamedGate Qasm.GateSX [] [decl] nullGateMod

test24 = TestCase (assertEqual "namedGateTransl: support for uncontrolled V gate (4/4)."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateV True [3] []))
    where decl = Cell "input_qwires" 2
          mod  = negateMod nullGateMod
          gate = Qasm.NamedGate Qasm.GateSX [] [decl] mod

test25 = TestCase (assertEqual "namedGateTransl: support for 4-controlled V gate."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateV False [1] ctrls))
    where ctrls = [Quip.Pos 5, Quip.Neg 4, Quip.Pos 3, Quip.Neg 2]
          decl1 = Cell "input_qwires" 4
          decl2 = Cell "input_qwires" 3
          decl3 = Cell "input_qwires" 2
          decl4 = Cell "input_qwires" 1
          decl5 = Cell "input_qwires" 0
          ops   = [decl1, decl2, decl3, decl4, decl5]
          mod   = addCtrlsToMod 1 $ addNegCtrlsToMod 1
                                  $ addCtrlsToMod 1
                                  $ addNegCtrlsToMod 1
                                  $ nullGateMod
          gate  = Qasm.NamedGate Qasm.GateSX [] ops mod

-----------------------------------------------------------------------------------------
-- Translating gates without named controlled instances (single configuration).

test26 = TestCase (assertEqual "namedGateTransl: support for the S gate."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateS False [1] []))
    where decl = Cell "input_qwires" 0
          gate = Qasm.NamedGate Qasm.GateS [] [decl] nullGateMod

test27 = TestCase (assertEqual "namedGateTransl: support for the T gate."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateT False [1] []))
    where decl = Cell "input_qwires" 0
          gate = Qasm.NamedGate Qasm.GateT [] [decl] nullGateMod

test28 = TestCase (assertEqual "namedGateTransl: support for the iX gate."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateIX False [1] []))
    where decl = Cell "input_qwires" 0
          gate = Qasm.NamedGate Qasm.GateQuipIX [] [decl] nullGateMod

test29 = TestCase (assertEqual "namedGateTransl: support for the omega gate."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateOmega False [1] []))
    where decl = Cell "input_qwires" 0
          gate = Qasm.NamedGate Qasm.GateQuipOmega [] [decl] nullGateMod

test30 = TestCase (assertEqual "namedGateTransl: support for the W gate."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateW False [1] []))
    where decl = Cell "input_qwires" 0
          gate = Qasm.NamedGate Qasm.GateQuipW [] [decl] nullGateMod

test31 = TestCase (assertEqual "namedGateTransl: support for the E gate."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateE False [1] []))
    where decl = Cell "input_qwires" 0
          gate = Qasm.NamedGate Qasm.GateQuipE [] [decl] nullGateMod

-----------------------------------------------------------------------------------------
-- Translating Toffoli gates.

test32 = TestCase (assertEqual "namedGateTransl: uncontrolled Toffoli gates (1/7)."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateX False [1] ctrls))
    where ctrls = [Quip.Pos 3, Quip.Pos 2]
          decl1 = Cell "input_qwires" 2
          decl2 = Cell "input_qwires" 1
          decl3 = Cell "input_qwires" 0
          ops   = [decl1, decl2, decl3]
          gate  = Qasm.NamedGate Qasm.GateCCX [] ops nullGateMod

test33 = TestCase (assertEqual "namedGateTransl: uncontrolled Toffoli gates (2/7)."
                               [AstGateStmt 1 neg, AstGateStmt 1 gate, AstGateStmt 1 neg]
                               (namedGateTransl qalloc Quip.GateX False [1] ctrls))
    where ctrls = [Quip.Neg 3, Quip.Pos 2]
          decl1 = Cell "input_qwires" 2
          decl2 = Cell "input_qwires" 1
          decl3 = Cell "input_qwires" 0
          ops   = [decl1, decl2, decl3]
          neg   = Qasm.NamedGate Qasm.GateX [] [decl1] nullGateMod
          gate  = Qasm.NamedGate Qasm.GateCCX [] ops nullGateMod

test34 = TestCase (assertEqual "namedGateTransl: uncontrolled Toffoli gates (3/7)."
                               [AstGateStmt 1 neg, AstGateStmt 1 gate, AstGateStmt 1 neg]
                               (namedGateTransl qalloc Quip.GateX False [1] ctrls))
    where ctrls = [Quip.Pos 3, Quip.Neg 2]
          decl1 = Cell "input_qwires" 2
          decl2 = Cell "input_qwires" 1
          decl3 = Cell "input_qwires" 0
          ops   = [decl1, decl2, decl3]
          neg   = Qasm.NamedGate Qasm.GateX [] [decl2] nullGateMod
          gate  = Qasm.NamedGate Qasm.GateCCX [] ops nullGateMod

test35 = TestCase (assertEqual "namedGateTransl: uncontrolled Toffoli gates (4/7)."
                               [AstGateStmt 1 neg2,
                                AstGateStmt 1 neg1,
                                AstGateStmt 1 gate,
                                AstGateStmt 1 neg1,
                                AstGateStmt 1 neg2]
                               (namedGateTransl qalloc Quip.GateX False [1] ctrls))
    where ctrls = [Quip.Neg 3, Quip.Neg 2]
          decl1 = Cell "input_qwires" 2
          decl2 = Cell "input_qwires" 1
          decl3 = Cell "input_qwires" 0
          ops   = [decl1, decl2, decl3]
          neg1  = Qasm.NamedGate Qasm.GateX [] [decl1] nullGateMod
          neg2  = Qasm.NamedGate Qasm.GateX [] [decl2] nullGateMod
          gate  = Qasm.NamedGate Qasm.GateCCX [] ops nullGateMod

test36 = TestCase (assertEqual "namedGateTransl: uncontrolled Toffoli gates (5/7)."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateX True [1] ctrls))
    where ctrls = [Quip.Pos 3, Quip.Pos 2]
          decl1 = Cell "input_qwires" 2
          decl2 = Cell "input_qwires" 1
          decl3 = Cell "input_qwires" 0
          ops   = [decl1, decl2, decl3]
          gate  = Qasm.NamedGate Qasm.GateCCX [] ops nullGateMod

test37 = TestCase (assertEqual "namedGateTransl: uncontrolled Toffoli gates (6/7)."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateX True [4] ctrls))
    where ctrls = [Quip.Pos 6, Quip.Pos 5]
          decl1 = Cell "input_qwires" 5
          decl2 = Cell "input_qwires" 4
          decl3 = Cell "input_qwires" 3
          ops   = [decl1, decl2, decl3]
          gate  = Qasm.NamedGate Qasm.GateCCX [] ops nullGateMod

test38 = TestCase (assertEqual "namedGateTransl: uncontrolled Toffoli gates (7/7)."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateX True [1] ctrls))
    where ctrls = [Quip.Pos 4, Quip.Pos 3, Quip.Pos 2]
          decl1 = Cell "input_qwires" 1
          decl2 = Cell "input_qwires" 3
          decl3 = Cell "input_qwires" 2
          decl4 = Cell "input_qwires" 0
          ops   = [decl1, decl2, decl3, decl4]
          mod   = addCtrlsToMod 1 $ nullGateMod
          gate  = Qasm.NamedGate Qasm.GateCCX [] ops mod

-----------------------------------------------------------------------------------------
-- Translating QMultiNot gates.

test39 = TestCase (assertEqual "namedGateTransl: QMultiNot (1/4)."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateQMultiNot False [1] []))
    where decl = Cell "input_qwires" 0
          gate = Qasm.NamedGate Qasm.GateX [] [decl] nullGateMod

test40 = TestCase (assertEqual "namedGateTransl: QMultiNot (2/4)."
                               [AstGateStmt 1 gate]
                               (namedGateTransl qalloc Quip.GateQMultiNot True [1] ctrls))
    where ctrls = [Quip.Pos 2]
          decl1 = Cell "input_qwires" 1
          decl2 = Cell "input_qwires" 0
          gate  = Qasm.NamedGate Qasm.GateCX [] [decl1, decl2] nullGateMod

test41 = TestCase (assertEqual "namedGateTransl: QMultiNot (3/4)."
                               [AstGateStmt 1 gate1,
                                AstGateStmt 1 gate2,
                                AstGateStmt 1 gate3]
                               (namedGateTransl qalloc Quip.GateQMultiNot False ins []))
    where ins   = [1, 2, 3]
          decl1 = Cell "input_qwires" 0
          decl2 = Cell "input_qwires" 1
          decl3 = Cell "input_qwires" 2
          gate1 = Qasm.NamedGate Qasm.GateX [] [decl1] nullGateMod
          gate2 = Qasm.NamedGate Qasm.GateX [] [decl2] nullGateMod
          gate3 = Qasm.NamedGate Qasm.GateX [] [decl3] nullGateMod

test42 = TestCase (assertEqual "namedGateTransl: QMultiNot (4/4)."
                               [AstGateStmt 1 gate1,
                                AstGateStmt 1 gate2,
                                AstGateStmt 1 gate3]
                               (namedGateTransl qalloc Quip.GateQMultiNot True ins ctrls))
    where ins   = [1, 2, 3]
          ctrls = [Quip.Pos 4]
          decl1 = Cell "input_qwires" 0
          decl2 = Cell "input_qwires" 1
          decl3 = Cell "input_qwires" 2
          decl4 = Cell "input_qwires" 3
          gate1 = Qasm.NamedGate Qasm.GateCX [] [decl4, decl1] nullGateMod
          gate2 = Qasm.NamedGate Qasm.GateCX [] [decl4, decl2] nullGateMod
          gate3 = Qasm.NamedGate Qasm.GateCX [] [decl4, decl3] nullGateMod

-----------------------------------------------------------------------------------------
-- Translating GPhase gates.

test43 = TestCase (assertEqual "Translating global phase gates without controls (1/3)."
                               [AstGateStmt 1 gate]
                               (translGPhase qalloc 1 []))
    where param = Times Pi $ DecFloat "1.0"
          gate  = GPhaseGate param [] nullGateMod

test44 = TestCase (assertEqual "Translating global phase gates without controls (2/3)."
                               [AstGateStmt 1 gate]
                               (translGPhase qalloc 0 []))
    where param = Times Pi $ DecFloat "0.0"
          gate  = GPhaseGate param [] nullGateMod

test45 = TestCase (assertEqual "Translating global phase gates without controls (3/3)."
                               [AstGateStmt 1 gate]
                               (translGPhase qalloc 2.3e-15 []))
    where param = Times Pi $ DecFloat "2.3e-15"
          gate  = GPhaseGate param [] nullGateMod

test46 = TestCase (assertEqual "Translating global phase gates with 1 control (1/2)."
                               [AstGateStmt 1 gate]
                               (translGPhase qalloc 1 ctrls))
    where param = Times Pi $ DecFloat "1.0"
          ctrls = [Quip.Pos 2]
          decl  = Cell "input_qwires" 1
          gate  = Qasm.NamedGate Qasm.GateP [param] [decl] nullGateMod

test47 = TestCase (assertEqual "Translating global phase gates with 1 control (2/2)."
                               [AstGateStmt 1 neg,
                                AstGateStmt 1 gate,
                                AstGateStmt 1 neg]
                               (translGPhase qalloc 2.3e-15 ctrls))
    where param = Times Pi $ DecFloat "2.3e-15"
          ctrls = [Quip.Neg 3]
          decl  = Cell "input_qwires" 2
          neg   = Qasm.NamedGate Qasm.GateX [] [decl] nullGateMod
          gate  = Qasm.NamedGate Qasm.GateP [param] [decl] nullGateMod

test48 = TestCase (assertEqual "Translating global phase gates with 2 controls (1/4)."
                               [AstGateStmt 1 gate]
                               (translGPhase qalloc 1 ctrls))
    where param = Times Pi $ DecFloat "1.0"
          ctrls = [Quip.Pos 2, Quip.Pos 3]
          decl1 = Cell "input_qwires" 1
          decl2 = Cell "input_qwires" 2
          gate  = Qasm.NamedGate Qasm.GateCP [param] [decl1, decl2] nullGateMod

test49 = TestCase (assertEqual "Translating global phase gates with 2 controls (2/4)."
                               [AstGateStmt 1 neg,
                                AstGateStmt 1 gate,
                                AstGateStmt 1 neg]
                               (translGPhase qalloc 2.3e-15 ctrls))
    where param = Times Pi $ DecFloat "2.3e-15"
          ctrls = [Quip.Neg 4, Quip.Pos 3]
          decl1 = Cell "input_qwires" 3
          decl2 = Cell "input_qwires" 2
          neg   = Qasm.NamedGate Qasm.GateX [] [decl1] nullGateMod
          gate  = Qasm.NamedGate Qasm.GateCP [param] [decl1, decl2] nullGateMod

test50 = TestCase (assertEqual "Translating global phase gates with 2 controls (3/4)."
                               [AstGateStmt 1 neg,
                                AstGateStmt 1 gate,
                                AstGateStmt 1 neg]
                               (translGPhase qalloc 0 ctrls))
    where param = Times Pi $ DecFloat "0.0"
          ctrls = [Quip.Pos 2, Quip.Neg 1]
          decl1 = Cell "input_qwires" 1
          decl2 = Cell "input_qwires" 0
          neg   = Qasm.NamedGate Qasm.GateX [] [decl2] nullGateMod
          gate  = Qasm.NamedGate Qasm.GateCP [param] [decl1, decl2] nullGateMod

test51 = TestCase (assertEqual "Translating global phase gates with 2 controls (4/4)."
                               [AstGateStmt 1 neg1,
                                AstGateStmt 1 neg2,
                                AstGateStmt 1 gate,
                                AstGateStmt 1 neg2,
                                AstGateStmt 1 neg1]
                               (translGPhase qalloc 0 ctrls))
    where param = Times Pi $ DecFloat "0.0"
          ctrls = [Quip.Neg 2, Quip.Neg 1]
          decl1 = Cell "input_qwires" 1
          decl2 = Cell "input_qwires" 0
          neg1  = Qasm.NamedGate Qasm.GateX [] [decl2] nullGateMod
          neg2  = Qasm.NamedGate Qasm.GateX [] [decl1] nullGateMod
          gate  = Qasm.NamedGate Qasm.GateCP [param] [decl1, decl2] nullGateMod

test52 = TestCase (assertEqual "Translating global phase gates with 3 controls (1/2)."
                               [AstGateStmt 1 gate]
                               (translGPhase qalloc 1 ctrls))
    where param = Times Pi $ DecFloat "1.0"
          ctrls = [Quip.Pos 3, Quip.Pos 2, Quip.Pos 1]
          decl1 = Cell "input_qwires" 0
          decl2 = Cell "input_qwires" 2
          decl3 = Cell "input_qwires" 1
          mod   = addCtrlsToMod 1 $ nullGateMod
          gate  = Qasm.NamedGate Qasm.GateCP [param] [decl1, decl2, decl3] mod

test53 = TestCase (assertEqual "Translating global phase gates with 3 controls (2/2)."
                               [AstGateStmt 1 gate]
                               (translGPhase qalloc 1 ctrls))
    where param = Times Pi $ DecFloat "1.0"
          ctrls = [Quip.Pos 3, Quip.Pos 2, Quip.Neg 5]
          decl1 = Cell "input_qwires" 4
          decl2 = Cell "input_qwires" 2
          decl3 = Cell "input_qwires" 1
          mod   = addNegCtrlsToMod 1 $ nullGateMod
          gate  = Qasm.NamedGate Qasm.GateCP [param] [decl1, decl2, decl3] mod

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
                                     TestLabel "GateY_Pos5_Neg4_Pos3_Neg2" test16,
                                     TestLabel "GateCX" test17,
                                     TestLabel "GateCZ" test18,
                                     TestLabel "GateCH" test19,
                                     TestLabel "GateCSwap" test20,
                                     TestLabel "GateV_NoCtrl_NoInv_Q1" test21,
                                     TestLabel "GateV_NoCtrl_NoInv_Q3" test22,
                                     TestLabel "GateV_NoCtrl_Inv_Q1" test23,
                                     TestLabel "GateV_NoCtrl_Inv_Q3" test24,
                                     TestLabel "GateV_Pos2_Pos3_Neg4_Neg5" test25,
                                     TestLabel "Gate_S" test26,
                                     TestLabel "Gate_T" test27,
                                     TestLabel "Gate_iX" test28,
                                     TestLabel "Gate_Omega" test29,
                                     TestLabel "Gate_W" test30,
                                     TestLabel "Gate_E" test31,
                                     TestLabel "Tof_PosPos_NoInv_Q3Q2Q1" test32,
                                     TestLabel "Tof_NegPos_NoInv_Q3Q2Q1" test33,
                                     TestLabel "Tof_PosNeg_NoInv_Q3Q2Q1" test34,
                                     TestLabel "Tof_NegNeg_NoInv_Q3Q2Q1" test35,
                                     TestLabel "Tof_PosPos_Inv_Q3Q2Q1" test36,
                                     TestLabel "Tof_PosPos_NoInv_Q6Q5Q3" test37,
                                     TestLabel "Tof_PosPosPos_NoInv_Q3Q2Q1" test38,
                                     TestLabel "QMultiNot_NoCtrls_NoInv_Q1" test39,
                                     TestLabel "QMultiNot_Pos2_Inv_Q2" test40,
                                     TestLabel "QMultiNot_NoCtrls_NoInv_Q1Q2Q3" test41,
                                     TestLabel "QMultiNot_Pos4_Inv_Q1Q2Q3" test42,
                                     TestLabel "GPhase_0Ctrl_1" test43,
                                     TestLabel "GPhase_0Ctrl_2" test44,
                                     TestLabel "GPhase_0Ctrl_3" test45,
                                     TestLabel "GPhase_1Ctrl_1" test46,
                                     TestLabel "GPhase_1Ctrl_2" test47,
                                     TestLabel "GPhase_2Ctrl_1" test48,
                                     TestLabel "GPhase_2Ctrl_2" test49,
                                     TestLabel "GPhase_2Ctrl_3" test50,
                                     TestLabel "GPhase_2Ctrl_3" test51,
                                     TestLabel "GPhase_3Ctrl_1" test52,
                                     TestLabel "GPhase_3Ctrl_1" test53]

main = defaultMain tests
