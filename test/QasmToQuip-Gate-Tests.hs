module Main where

import qualified Data.IntMap.Strict as IntMap
import Data.Maybe
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.Gate
  ( addCtrlsToMod
  , addNegCtrlsToMod
  , negateMod
  , nullGateMod
  )
import qualified LinguaQuanta.Qasm.GateName as Qasm
import LinguaQuanta.QasmToQuip.Wire
import LinguaQuanta.Quip.Gate
import qualified LinguaQuanta.Quip.GateName as Quip

-----------------------------------------------------------------------------------------
-- Context

-- Allocations:
alloc0 = initialAllocations
alloc1 = fromJust $ allocate QWire "decl1" Nothing  alloc0
alloc2 = fromJust $ allocate CWire "decl2" (Just 5) alloc1
alloc3 = fromJust $ allocate QWire "decl3" (Just 4) alloc2
alloc4 = fromJust $ allocate CWire "decl4" Nothing  alloc3

-- Operands:
decl1       = Scalar "decl1" idx
decl2at idx = Cell "decl2" idx
decl3at idx = Cell "decl3" idx
decl4       = Scalar "decl4" idx

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
                              (NamedGate Quip.GateS False [0] [])
                              (namedGateTransl alloc4 Qasm.GateS [decl1] mod0))

test2 = TestCase (assertEqual "Translation of an S gate with inversion."
                              (NamedGate Quip.GateS True [0] [])
                              (namedGateTransl alloc4 Qasm.GateS [decl1] mod1))

test3 = TestCase (assertEqual "Translation of an S gate with 2 ctrls (1/2)."
                              (NamedGate Quip.GateS False ins ctrls)
                              (namedGateTransl alloc4 Qasm.GateS ops mod2))
    where ops   = [decl1, decl2at 0, decl2at 3]
          ins   = [9]
          ctrls = [Pos 0, Pos 2]

test4 = TestCase (assertEqual "Translation of an S gate with 2 ctrls (2/2)."
                              (NamedGate Quip.GateS False ins ctrls)
                              (namedGateTransl alloc4 Qasm.GateS ops mod3))
    where ops   = [decl1, decl2at 0, decl2at 3]
          ins   = [9]
          ctrls = [Pos 0, Neg 2]

test5 = TestCase (assertEqual "Translation of an S gate with 2 ctrls and inversion (1/2)."
                              (NamedGate Quip.GateS True ins ctrls)
                              (namedGateTransl alloc4 Qasm.GateS ops mod4))
    where ops   = [decl1, decl2at 0, decl2at 3]
          ins   = [9]
          ctrls = [Pos 0, Pos 2]

test6 = TestCase (assertEqual "Translation of an S gate with 2 ctrls and inversion (2/2)."
                              (NamedGate Quip.GateS True ins ctrls)
                              (namedGateTransl alloc4 Qasm.GateS ops mod5))
    where ops   = [decl1, decl2at 0, decl2at 3]
          ins   = [9]
          ctrls = [Pos 0, Neg 2]

-----------------------------------------------------------------------------------------
-- Translating simple named gates (single configuration)

test7 = TestCase (assertEqual "Translation of a Pauli-X gate without modifiers."
                              (NamedGate Quip.GateX False [0] [])
                              (namedGateTransl alloc4 Qasm.GateX [decl1] mod0))

test8 = TestCase (assertEqual "Translation of a Pauli-Y gate without modifiers."
                              (NamedGate Quip.GateY False [0] [])
                              (namedGateTransl alloc4 Qasm.GateY [decl1] mod0))

test9 = TestCase (assertEqual "Translation of a Pauli-Z gate without modifiers."
                              (NamedGate Quip.GateZ False [0] [])
                              (namedGateTransl alloc4 Qasm.GateZ [decl1] mod0))

test10 = TestCase (assertEqual "Translation of a Hadamard gate without modifiers."
                               (NamedGate Quip.GateH False [0] [])
                               (namedGateTransl alloc4 Qasm.GateH [decl1] mod0))

test11 = TestCase (assertEqual "Translation of a Swap gate without modifiers."
                               (NamedGate Quip.GateSwap False [0, 9] [])
                               (namedGateTransl alloc4 Qasm.GateH ops mod0))
    where ops = [decl1, decl2at 3]

test12 = TestCase (assertEqual "Translation of an SX gate without modifiers."
                               (NamedGate Quip.GateV False [0] [])
                               (namedGateTransl alloc4 Qasm.GateSX [decl1] mod0))

test13 = TestCase (assertEqual "Translation of a T gate without modifiers."
                               (NamedGate Quip.GateT False [0] [])
                               (namedGateTransl alloc4 Qasm.GateT [decl1] mod0))

test14 = TestCase (assertEqual "Translation of an iX gate without modifiers."
                               (NamedGate Quip.GateIX False [0] [])
                               (namedGateTransl alloc4 Qasm.GateQuipIX [decl1] mod0))

test15 = TestCase (assertEqual "Translation of an omega gate without modifiers."
                               (NamedGate Quip.GateOmega False [0] [])
                               (namedGateTransl alloc4 Qasm.GateQuipOmega [decl1] mod0))

test16 = TestCase (assertEqual "Translation of an E gate without modifiers."
                               (NamedGate Quip.GateE False [0] [])
                               (namedGateTransl alloc4 Qasm.GateQuipE [decl1] mod0))

test17 = TestCase (assertEqual "Translation of a W gate without modifiers."
                               (NamedGate Quip.GateW False [0] [])
                               (namedGateTransl alloc4 Qasm.GateQuipW [decl1] mod0))

-----------------------------------------------------------------------------------------
-- Translating named inverse gates.

test18 = TestCase (assertEqual "Translation of an Sdg gate without modifiers."
                               (NamedGate Quip.GateS True [0] [])
                               (namedGateTransl alloc4 Qasm.GateSdg [decl1] mod0))

test19 = TestCase (assertEqual "Translation of an Sdg gate with inversion."
                               (NamedGate Quip.GateS False [0] [])
                               (namedGateTransl alloc4 Qasm.GateSdg [decl1] mod1))

test20 = TestCase (assertEqual "Translation of an Sdg gate with 2 ctrls (1/2)."
                               (NamedGate Quip.GateS True ins ctrls)
                               (namedGateTransl alloc4 Qasm.GateSdg ops mod2))
    where ops   = [decl1, decl2at 0, decl2at 3]
          ins   = [9]
          ctrls = [Pos 0, Pos 2]

test21 = TestCase (assertEqual "Translation of an Sdg gate with 2 ctrls (2/2)."
                               (NamedGate Quip.GateS True ins ctrls)
                               (namedGateTransl alloc4 Qasm.GateSdg ops mod3))
    where ops   = [decl1, decl2at 0, decl2at 3]
          ins   = [9]
          ctrls = [Pos 0, Neg 2]

test22 = TestCase (assertEqual "Translation of an Sdg gate with 2 ctrls and inversion (1/2)."
                               (NamedGate Quip.GateS False ins ctrls)
                               (namedGateTransl alloc4 Qasm.GateSdg ops mod4))
    where ops   = [decl1, decl2at 0, decl2at 3]
          ins   = [9]
          ctrls = [Pos 0, Pos 2]

test23 = TestCase (assertEqual "Translation of an Sdg gate with 2 ctrls and inversion (2/2)."
                               (NamedGate Quip.GateS False ins ctrls)
                               (namedGateTransl alloc4 Qasm.GateSdg ops mod5))
    where ops   = [decl1, decl2at 0, decl2at 3]
          ins   = [9]
          ctrls = [Pos 0, Neg 2]

test24 = TestCase (assertEqual "Translation of an Tdg gate without modifiers."
                               (NamedGate Quip.GateT True [0] [])
                               (namedGateTransl alloc4 Qasm.GateTdg [decl1] mod0))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestCase test1 "GateS_1",
                                     TestCase test2 "GateS_2",
                                     TestCase test3 "GateS_3",
                                     TestCase test4 "GateS_4",
                                     TestCase test5 "GateS_5",
                                     TestCase test6 "GateS_6",
                                     TestCase test7 "GateX",
                                     TestCase test8 "GateY",
                                     TestCase test9 "GateZ",
                                     TestCase test10 "GateH",
                                     TestCase test11 "GateSwap",
                                     TestCase test12 "GateSX",
                                     TestCase test13 "GateT",
                                     TestCase test14 "GateIX",
                                     TestCase test15 "GateOmega",
                                     TestCase test16 "GateE",
                                     TestCase test17 "GateW",
                                     TestCase test18 "GateSdg_1",
                                     TestCase test19 "GateSdg_2",
                                     TestCase test20 "GateSdg_3",
                                     TestCase test21 "GateSdg_4",
                                     TestCase test22 "GateSdg_5",
                                     TestCase test23 "GateSdg_6",
                                     TestCase test24 "GateTdg"]

main = defaultMain tests
