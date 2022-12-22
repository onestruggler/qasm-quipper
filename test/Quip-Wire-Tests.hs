module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Quip.Wire

import qualified Data.IntMap.Strict as IntMap

-----------------------------------------------------------------------------------------
-- countImpl

map0 = IntMap.empty
map1 = IntMap.insert 1 QWire map0
map2 = IntMap.insert 7 CWire map1
map3 = IntMap.insert 5 QWire map2
map4 = IntMap.insert 0 QWire map3
map5 = IntMap.insert 9 CWire map4
map6 = IntMap.insert 3 QWire map5

test1 = TestCase (assertEqual "countQWires (1/7)."
                              0
                              (countQWires map0))

test2 = TestCase (assertEqual "countQWires (2/7)."
                              1
                              (countQWires map1))

test3 = TestCase (assertEqual "countQWires (3/7)."
                              1
                              (countQWires map2))

test4 = TestCase (assertEqual "countQWires (4/7)."
                              2
                              (countQWires map3))

test5 = TestCase (assertEqual "countQWires (5/7)."
                              3
                              (countQWires map4))

test6 = TestCase (assertEqual "countQWires (6/7)."
                              3
                              (countQWires map5))

test7 = TestCase (assertEqual "countQWires (7/7)."
                              4
                              (countQWires map6))

test8 = TestCase (assertEqual "countCWires (1/7)."
                              0
                              (countCWires map0))

test9 = TestCase (assertEqual "countCWires (2/7)."
                              0
                              (countCWires map1))

test10 = TestCase (assertEqual "countCWires (3/7)."
                               1
                               (countCWires map2))

test11 = TestCase (assertEqual "countCWires (4/7)."
                               1
                               (countCWires map3))

test12 = TestCase (assertEqual "countCWires (5/7)."
                               1
                               (countCWires map4))

test13 = TestCase (assertEqual "countCWires (6/7)."
                               2
                               (countCWires map5))

test14 = TestCase (assertEqual "countCWires (7/7)."
                               2
                               (countCWires map6))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "countQWires_0" test1,
                                     TestLabel "countQWires_1" test2,
                                     TestLabel "countQWires_2" test3,
                                     TestLabel "countQWires_3" test4,
                                     TestLabel "countQWires_4" test5,
                                     TestLabel "countQWires_5" test6,
                                     TestLabel "countQWires_6" test7,
                                     TestLabel "countCWires_0" test8,
                                     TestLabel "countCWires_1" test9,
                                     TestLabel "countCWires_2" test10,
                                     TestLabel "countCWires_3" test11,
                                     TestLabel "countCWires_4" test12,
                                     TestLabel "countCWires_5" test13,
                                     TestLabel "countCWires_6" test14]

main = defaultMain tests
