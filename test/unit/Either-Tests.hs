module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Either

-----------------------------------------------------------------------------------------
-- expandLeft

left1 :: Either Int String
left1 = Left 1

left2 :: Either Int String
left2 = Left 5

right1 :: Either Int String
right1 = Right "a" :: Either Int String

fn1 :: Int -> Either Int String
fn1 1 = Right "1"
fn1 n = Left $ n + 1

test1 = TestCase (assertEqual "expandLeft handles Left values (1/2)."
                              (Right "1" :: Either Int String)
                              (expandLeft left1 fn1))

test2 = TestCase (assertEqual "expandLeft handles Left values (2/2)."
                              (Left 6 :: Either Int String)
                              (expandLeft left2 fn1))

test3 = TestCase (assertEqual "expandLeft handles Right values."
                              right1
                              (expandLeft right1 fn1))

-----------------------------------------------------------------------------------------
-- leftMap

test4 = TestCase (assertEqual "leftMap handles empty lists."
                              (Left [] :: Either [Int] String)
                              (leftMap fn1 []))

test5 = TestCase (assertEqual "leftMap handles one element lists."
                              (Left [8] :: Either [Int] String)
                              (leftMap fn1 [7]))

test6 = TestCase (assertEqual "leftMap handles many element lists."
                              (Left [3, 5, 7] :: Either [Int] String)
                              (leftMap fn1 [2, 4, 6]))

test7 = TestCase (assertEqual "leftMap handles exceptions."
                              (Right "1" :: Either [Int] String)
                              (leftMap fn1 [7, 5, 3, 1, 3, 5, 7]))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "expandLeft_Left1" test1,
                                     TestLabel "expandLeft_Left2" test2,
                                     TestLabel "expandLeft_Right" test3,
                                     TestLabel "leftMap_Empty" test4,
                                     TestLabel "leftMap_Singleton" test5,
                                     TestLabel "leftMap_LongList" test6,
                                     TestLabel "leftMap_Exception" test6]

main = defaultMain tests
