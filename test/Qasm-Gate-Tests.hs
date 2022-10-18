module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Qasm.Gate
import Qasm.GateName
import Qasm.Language

-----------------------------------------------------------------------------------------
-- Gate Modifiers

mod0 = nullGateMod
mod1 = negateMod mod0
mod2 = addCtrlsToMod 2 mod1
mod3 = addNegCtrlsToMod 3 mod2
mod4 = negateMod mod3
mod5 = addCtrlsToMod 1 mod4

test1 = TestCase (assertEqual "Building gate modifiers (1/6)."
                              (GateMod False [])
                              mod0)

test2 = TestCase (assertEqual "Building gate modifiers (2/6)."
                              (GateMod True [])
                              mod1)

test3 = TestCase (assertEqual "Building gate modifiers (3/6)."
                              (GateMod True [Pos, Pos])
                              mod2)

test4 = TestCase (assertEqual "Building gate modifiers (4/6)."
                              (GateMod True [Neg, Neg, Neg, Pos, Pos])
                              mod3)

test5 = TestCase (assertEqual "Building gate modifiers (5/6)."
                              (GateMod False [Neg, Neg, Neg, Pos, Pos])
                              mod4)

test6 = TestCase (assertEqual "Building gate modifiers (6/6)."
                              (GateMod False [Pos, Neg, Neg, Neg, Pos, Pos])
                              mod5)

-----------------------------------------------------------------------------------------
-- Gate Construction

pidiv2 = Div Pi (DecInt "2")
params = [Pi, pidiv2]
inputs = [QVar "q"]

gate0A = NamedGate GateX params inputs nullGateMod
gate1A = invert gate0A
gate2A = addCtrls 2 gate1A
gate3A = addNegCtrls 3 gate2A
gate4A = invert gate3A
gate5A = addCtrls 1 gate4A

test7 = TestCase (assertEqual "Building named gates (1/6)."
                              (NamedGate GateX params inputs mod0)
                              gate0A)

test8 = TestCase (assertEqual "Building named gates (2/6)."
                              (NamedGate GateX params inputs mod1)
                              gate1A)

test9 = TestCase (assertEqual "Building named gates (3/6)."
                              (NamedGate GateX params inputs mod2)
                              gate2A)

test10 = TestCase (assertEqual "Building named gates (4/6)."
                               (NamedGate GateX params inputs mod3)
                               gate3A)

test11 = TestCase (assertEqual "Building named gates (5/6)."
                               (NamedGate GateX params inputs mod4)
                               gate4A)

test12 = TestCase (assertEqual "Building named gates (6/6)."
                               (NamedGate GateX params inputs mod5)
                               gate5A)

gate0B = GPhaseGate pidiv2 inputs nullGateMod
gate1B = invert gate0B
gate2B = addCtrls 2 gate1B
gate3B = addNegCtrls 3 gate2B
gate4B = invert gate3B
gate5B = addCtrls 1 gate4B

test13 = TestCase (assertEqual "Building phase gates (1/6)."
                               (GPhaseGate pidiv2 inputs mod0)
                               gate0B)

test14 = TestCase (assertEqual "Building phase gates (2/6)."
                               (GPhaseGate pidiv2 inputs mod1)
                               gate1B)

test15 = TestCase (assertEqual "Building phase gates (3/6)."
                               (GPhaseGate pidiv2 inputs mod2)
                               gate2B)

test16 = TestCase (assertEqual "Building phase gates (4/6)."
                               (GPhaseGate pidiv2 inputs mod3)
                               gate3B)

test17 = TestCase (assertEqual "Building phase gates (5/6)."
                               (GPhaseGate pidiv2 inputs mod4)
                               gate4B)

test18 = TestCase (assertEqual "Building phase gates (6/6)."
                               (GPhaseGate pidiv2 inputs mod5)
                               gate5B)

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "GateMod_1" test1,
                                     TestLabel "GateMod_1" test2,
                                     TestLabel "GateMod_1" test3,
                                     TestLabel "GateMod_1" test4,
                                     TestLabel "GateMod_1" test5,
                                     TestLabel "GateMod_1" test6,
                                     TestLabel "NamedGate_1" test7,
                                     TestLabel "NamedGate_2" test8,
                                     TestLabel "NamedGate_3" test9,
                                     TestLabel "NamedGate_4" test10,
                                     TestLabel "NamedGate_5" test11,
                                     TestLabel "NamedGate_6" test12,
                                     TestLabel "PhaseGate_1" test13,
                                     TestLabel "PhaseGate_2" test14,
                                     TestLabel "PhaseGate_3" test15,
                                     TestLabel "PhaseGate_4" test16,
                                     TestLabel "PhaseGate_5" test17,
                                     TestLabel "PhaseGate_6" test18]

main = defaultMain tests
