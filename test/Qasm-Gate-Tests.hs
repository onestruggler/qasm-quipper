module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Qasm.Gate

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
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "GateMod_1" test1,
                                     TestLabel "GateMod_1" test2,
                                     TestLabel "GateMod_1" test3,
                                     TestLabel "GateMod_1" test4,
                                     TestLabel "GateMod_1" test5,
                                     TestLabel "GateMod_1" test6]

main = defaultMain tests
