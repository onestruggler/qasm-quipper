module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.List

-----------------------------------------------------------------------------------------
-- splitAtFirst

test1 = TestCase (assertEqual "splitAtFirst: match in middle of word."
                              ("qwerty", "asd")
                              (splitAtFirst (== 'a') "qwertyaasd"))

test2 = TestCase (assertEqual "splitAtFirst: match at end of word."
                              ("qwerty", "")
                              (splitAtFirst (== 'b') "qwertyb"))

test3 = TestCase (assertEqual "splitAtFirst: match at start of word."
                              ("", "cqwerty")
                              (splitAtFirst (== 'c') "ccqwerty"))

test4 = TestCase (assertEqual "splitAtFirst: no match."
                              ("qwerty", "")
                              (splitAtFirst (== 'c') "qwerty"))

test5 = TestCase (assertEqual "splitAtFirst: integer type."
                              ([1, 2, 3, 4, 5], [7, 8, 9, 10])
                              (splitAtFirst (> 5) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "splitAtFirst_1" test1,
                                     TestLabel "splitAtFirst_2" test2,
                                     TestLabel "splitAtFirst_3" test3,
                                     TestLabel "splitAtFirst_4" test4,
                                     TestLabel "splitAtFirst_5" test5]

main = defaultMain tests
