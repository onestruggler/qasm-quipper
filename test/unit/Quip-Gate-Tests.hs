module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Quip.Gate

-----------------------------------------------------------------------------------------
-- toWires

test1 = TestCase (assertEqual "toWires handles an empty list of controls."
                              []
                              (toWires []))

test2 = TestCase (assertEqual "toWires handles a single positive control."
                              [5]
                              (toWires [Pos 5]))

test3 = TestCase (assertEqual "toWires handles a single positive control."
                              [9]
                              (toWires [Neg 9]))

test4 = TestCase (assertEqual "toWires handles a list of mixed controls."
                              [7, 3, 12, 4, 22, 55]
                              (toWires [Pos 7, Neg 3, Pos 12, Neg 4, Neg 22, Pos 55]))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "toWires_Empty" test1,
                                     TestLabel "toWires_Pos" test2,
                                     TestLabel "toWires_Neg" test3,
                                     TestLabel "toWires_Mixed" test4]

main = defaultMain tests
