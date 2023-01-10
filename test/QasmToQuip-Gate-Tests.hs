module Main where

import qualified Data.IntMap.Strict as IntMap
import Data.Maybe
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.Gate
  ( Operand(..)
  , addCtrlsToMod
  , addNegCtrlsToMod
  , negateMod
  , nullGateMod
  )
import qualified LinguaQuanta.Qasm.GateName as Qasm
import LinguaQuanta.QasmToQuip.Gate
import LinguaQuanta.QasmToQuip.Wire
  ( initialAllocations
  , allocate
  )
import LinguaQuanta.Quip.Gate
import qualified LinguaQuanta.Quip.GateName as Quip
import LinguaQuanta.Quip.Wire

-----------------------------------------------------------------------------------------
-- Context

-- Allocations:
alloc0 = initialAllocations
alloc1 = fromJust $ allocate QWire "decl1" Nothing  alloc0
alloc2 = fromJust $ allocate CWire "decl2" (Just 5) alloc1
alloc3 = fromJust $ allocate QWire "decl3" (Just 4) alloc2
alloc4 = fromJust $ allocate CWire "decl4" Nothing  alloc3

-- Operands:
decl1       = Scalar "decl1"
decl2at idx = Cell "decl2" idx
decl3at idx = Cell "decl3" idx
decl4       = Scalar "decl4"

-- Modifiers:
mod0 = nullGateMod
mod1 = negateMod $ nullGateMod
mod2 = addCtrlsToMod 2 $ nullGateMod
mod3 = addCtrlsToMod 1 $ addNegCtrlsToMod 1 $ nullGateMod
mod4 = negateMod $ mod2
mod5 = negateMod $ mod3

-----------------------------------------------------------------------------------------
-- Translating the S gate (many configurations)

test1 = TestCase (assertEqual "Translation of an S gate without modifiers."
                              [NamedGate Quip.GateS False [0] []]
                              (namedGateTransl alloc4 Qasm.GateS [decl1] mod0))

test2 = TestCase (assertEqual "Translation of an S gate with inversion."
                              [NamedGate Quip.GateS True [0] []]
                              (namedGateTransl alloc4 Qasm.GateS [decl1] mod1))

test3 = TestCase (assertEqual "Translation of an S gate with 2 ctrls (1/2)."
                              [NamedGate Quip.GateS False ins ctrls]
                              (namedGateTransl alloc4 Qasm.GateS ops mod2))
    where ops   = [decl1, decl2at 0, decl3at 3]
          ins   = [9]
          ctrls = [Pos 0, Pos 1]

test4 = TestCase (assertEqual "Translation of an S gate with 2 ctrls (2/2)."
                              [NamedGate Quip.GateS False ins ctrls]
                              (namedGateTransl alloc4 Qasm.GateS ops mod3))
    where ops   = [decl1, decl2at 0, decl3at 3]
          ins   = [9]
          ctrls = [Pos 0, Neg 1]

test5 = TestCase (assertEqual "Translation of an S gate with 2 ctrls and inversion (1/2)."
                              [NamedGate Quip.GateS True ins ctrls]
                              (namedGateTransl alloc4 Qasm.GateS ops mod4))
    where ops   = [decl1, decl2at 0, decl3at 3]
          ins   = [9]
          ctrls = [Pos 0, Pos 1]

test6 = TestCase (assertEqual "Translation of an S gate with 2 ctrls and inversion (2/2)."
                              [NamedGate Quip.GateS True ins ctrls]
                              (namedGateTransl alloc4 Qasm.GateS ops mod5))
    where ops   = [decl1, decl2at 0, decl3at 3]
          ins   = [9]
          ctrls = [Pos 0, Neg 1]

-----------------------------------------------------------------------------------------
-- Translating simple named gates (single configuration)

test7 = TestCase (assertEqual "Translation of a Pauli-X gate without modifiers."
                              [NamedGate Quip.GateX False [0] []]
                              (namedGateTransl alloc4 Qasm.GateX [decl1] mod0))

test8 = TestCase (assertEqual "Translation of a Pauli-Y gate without modifiers."
                              [NamedGate Quip.GateY False [0] []]
                              (namedGateTransl alloc4 Qasm.GateY [decl1] mod0))

test9 = TestCase (assertEqual "Translation of a Pauli-Z gate without modifiers."
                              [NamedGate Quip.GateZ False [0] []]
                              (namedGateTransl alloc4 Qasm.GateZ [decl1] mod0))

test10 = TestCase (assertEqual "Translation of a Hadamard gate without modifiers."
                               [NamedGate Quip.GateH False [0] []]
                               (namedGateTransl alloc4 Qasm.GateH [decl1] mod0))

test11 = TestCase (assertEqual "Translation of a Swap gate without modifiers."
                               [NamedGate Quip.GateSwap False [0, 4] []]
                               (namedGateTransl alloc4 Qasm.GateSwap ops mod0))
    where ops = [decl1, decl2at 3]

test12 = TestCase (assertEqual "Translation of an SX gate without modifiers."
                               [NamedGate Quip.GateV False [0] []]
                               (namedGateTransl alloc4 Qasm.GateSX [decl1] mod0))

test13 = TestCase (assertEqual "Translation of a T gate without modifiers."
                               [NamedGate Quip.GateT False [0] []]
                               (namedGateTransl alloc4 Qasm.GateT [decl1] mod0))

test14 = TestCase (assertEqual "Translation of an iX gate without modifiers."
                               [NamedGate Quip.GateIX False [0] []]
                               (namedGateTransl alloc4 Qasm.GateQuipIX [decl1] mod0))

test15 = TestCase (assertEqual "Translation of an omega gate without modifiers."
                               [NamedGate Quip.GateOmega False [0] []]
                               (namedGateTransl alloc4 Qasm.GateQuipOmega [decl1] mod0))

test16 = TestCase (assertEqual "Translation of an E gate without modifiers."
                               [NamedGate Quip.GateE False [0] []]
                               (namedGateTransl alloc4 Qasm.GateQuipE [decl1] mod0))

test17 = TestCase (assertEqual "Translation of a W gate without modifiers."
                               [NamedGate Quip.GateW False [0] []]
                               (namedGateTransl alloc4 Qasm.GateQuipW [decl1] mod0))

-----------------------------------------------------------------------------------------
-- Translating named inverse gates.

test18 = TestCase (assertEqual "Translation of an Sdg gate without modifiers."
                               [NamedGate Quip.GateS True [0] []]
                               (namedGateTransl alloc4 Qasm.GateSdg [decl1] mod0))

test19 = TestCase (assertEqual "Translation of an Sdg gate with inversion."
                               [NamedGate Quip.GateS False [0] []]
                               (namedGateTransl alloc4 Qasm.GateSdg [decl1] mod1))

test20 = TestCase (assertEqual "Translation of an Sdg gate with 2 ctrls (1/2)."
                               [NamedGate Quip.GateS True ins ctrls]
                               (namedGateTransl alloc4 Qasm.GateSdg ops mod2))
    where ops   = [decl1, decl2at 0, decl3at 3]
          ins   = [9]
          ctrls = [Pos 0, Pos 1]

test21 = TestCase (assertEqual "Translation of an Sdg gate with 2 ctrls (2/2)."
                               [NamedGate Quip.GateS True ins ctrls]
                               (namedGateTransl alloc4 Qasm.GateSdg ops mod3))
    where ops   = [decl1, decl2at 0, decl3at 3]
          ins   = [9]
          ctrls = [Pos 0, Neg 1]

test22 = TestCase (assertEqual "Translation of an Sdg gate with 2 ctrls and inversion (1/2)."
                               [NamedGate Quip.GateS False ins ctrls]
                               (namedGateTransl alloc4 Qasm.GateSdg ops mod4))
    where ops   = [decl1, decl2at 0, decl3at 3]
          ins   = [9]
          ctrls = [Pos 0, Pos 1]

test23 = TestCase (assertEqual "Translation of an Sdg gate with 2 ctrls and inversion (2/2)."
                               [NamedGate Quip.GateS False ins ctrls]
                               (namedGateTransl alloc4 Qasm.GateSdg ops mod5))
    where ops   = [decl1, decl2at 0, decl3at 3]
          ins   = [9]
          ctrls = [Pos 0, Neg 1]

test24 = TestCase (assertEqual "Translation of an Tdg gate without modifiers."
                               [NamedGate Quip.GateT True [0] []]
                               (namedGateTransl alloc4 Qasm.GateTdg [decl1] mod0))

-----------------------------------------------------------------------------------------
-- Translating the CX gate (many configurations)

test25 = TestCase (assertEqual "Translation of a CX gate without modifiers."
                               [NamedGate Quip.GateX False ins ctrls]
                               (namedGateTransl alloc4 Qasm.GateCX ops mod0))
    where ops   = [decl1, decl4]
          ins   = [10]
          ctrls = [Pos 0]

test26 = TestCase (assertEqual "Translation of a CX gate with inversion."
                               [NamedGate Quip.GateX False ins ctrls]
                               (namedGateTransl alloc4 Qasm.GateCX ops mod1))
    where ops   = [decl1, decl4]
          ins   = [10]
          ctrls = [Pos 0]

test27 = TestCase (assertEqual "Translation of a CX gate with 2 ctrls (1/2)."
                                [NamedGate Quip.GateX False ins ctrls]
                                (namedGateTransl alloc4 Qasm.GateCX ops mod2))
    where ops   = [decl2at 0, decl3at 3, decl1, decl4]
          ins   = [10]
          ctrls = [Pos 0, Pos 1, Pos 9]

test28 = TestCase (assertEqual "Translation of a CX gate with 2 ctrls (2/2)."
                                [NamedGate Quip.GateX False ins ctrls]
                                (namedGateTransl alloc4 Qasm.GateCX ops mod3))
    where ops   = [decl2at 0, decl3at 3, decl1, decl4]
          ins   = [10]
          ctrls = [Pos 0, Pos 1, Neg 9]

test29 = TestCase (assertEqual "Translation of a CX gate with 2 ctrls and inversion (1/2)."
                                [NamedGate Quip.GateX False ins ctrls]
                                (namedGateTransl alloc4 Qasm.GateCX ops mod4))
    where ops   = [decl2at 0, decl3at 3, decl1, decl4]
          ins   = [10]
          ctrls = [Pos 0, Pos 1, Pos 9]

test30 = TestCase (assertEqual "Translation of a CX gate with 2 ctrls and inversion (2/2)."
                                [NamedGate Quip.GateX False ins ctrls]
                                (namedGateTransl alloc4 Qasm.GateCX ops mod5))
    where ops   = [decl2at 0, decl3at 3, decl1, decl4]
          ins   = [10]
          ctrls = [Pos 0, Pos 1, Neg 9]

-----------------------------------------------------------------------------------------
-- Translating singly controlled gates (single configuration)

test31 = TestCase (assertEqual "Translation of a CY gate without modifiers."
                               [NamedGate Quip.GateY False ins ctrls]
                               (namedGateTransl alloc4 Qasm.GateCY ops mod0))
    where ops   = [decl1, decl4]
          ins   = [10]
          ctrls = [Pos 0]

test32 = TestCase (assertEqual "Translation of a CZ gate without modifiers."
                               [NamedGate Quip.GateZ False ins ctrls]
                               (namedGateTransl alloc4 Qasm.GateCZ ops mod0))
    where ops   = [decl1, decl4]
          ins   = [10]
          ctrls = [Pos 0]

test33 = TestCase (assertEqual "Translation of a CH gate without modifiers."
                               [NamedGate Quip.GateH False ins ctrls]
                               (namedGateTransl alloc4 Qasm.GateCH ops mod0))
    where ops   = [decl1, decl4]
          ins   = [10]
          ctrls = [Pos 0]

test34 = TestCase (assertEqual "Translation of a CSwap gate without modifiers."
                               [NamedGate Quip.GateSwap False ins ctrls]
                               (namedGateTransl alloc4 Qasm.GateCSwap ops mod0))
    where ops   = [decl1, decl4, decl2at 0]
          ins   = [10, 1]
          ctrls = [Pos 0]

-----------------------------------------------------------------------------------------
-- Translating the Toffoli gate (single configuration)

test35 = TestCase (assertEqual "Translation of a CCX gate with 2 ctrls and inversion."
                                [NamedGate Quip.GateX False ins ctrls]
                                (namedGateTransl alloc4 Qasm.GateCCX ops mod5))
    where ops   = [decl1, decl2at 0, decl2at 1, decl2at 2, decl2at 3]
          ins   = [4]
          ctrls = [Pos 2, Pos 3, Pos 0, Neg 1]

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "GateS_1" test1,
                                     TestLabel "GateS_2" test2,
                                     TestLabel "GateS_3" test3,
                                     TestLabel "GateS_4" test4,
                                     TestLabel "GateS_5" test5,
                                     TestLabel "GateS_6" test6,
                                     TestLabel "GateX" test7,
                                     TestLabel "GateY" test8,
                                     TestLabel "GateZ" test9,
                                     TestLabel "GateH" test10,
                                     TestLabel "GateSwap" test11,
                                     TestLabel "GateSX" test12,
                                     TestLabel "GateT" test13,
                                     TestLabel "GateIX" test14,
                                     TestLabel "GateOmega" test15,
                                     TestLabel "GateE" test16,
                                     TestLabel "GateW" test17,
                                     TestLabel "GateSdg_1" test18,
                                     TestLabel "GateSdg_2" test19,
                                     TestLabel "GateSdg_3" test20,
                                     TestLabel "GateSdg_4" test21,
                                     TestLabel "GateSdg_5" test22,
                                     TestLabel "GateSdg_6" test23,
                                     TestLabel "GateTdg" test24,
                                     TestLabel "GateCX_1" test25,
                                     TestLabel "GateCX_2" test26,
                                     TestLabel "GateCX_3" test27,
                                     TestLabel "GateCX_4" test28,
                                     TestLabel "GateCX_5" test29,
                                     TestLabel "GateCX_6" test30,
                                     TestLabel "GateCY" test31,
                                     TestLabel "GateCZ" test32,
                                     TestLabel "GateCH" test33,
                                     TestLabel "GateCSwap" test34,
                                     TestLabel "GateToffoli" test35]

main = defaultMain tests
