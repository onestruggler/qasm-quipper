module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Utils

-----------------------------------------------------------------------------------------
-- maybeWrap

test1 = TestCase (assertEqual "maybeWrap supports constructor Nothing."
                              (Nothing :: Maybe [Int])
                              (maybeWrap Nothing))

test2 = TestCase (assertEqual "maybeWrap supports constructor Just (type Int)."
                              (Just [5] :: Maybe [Int])
                              (maybeWrap (Just 5 :: Maybe Int)))

test3 = TestCase (assertEqual "maybeWrap supports constructor Just (type String)."
                              (Just ["Hello"] :: Maybe [String])
                              (maybeWrap (Just "Hello" :: Maybe String)))

-----------------------------------------------------------------------------------------
-- maybeAppend

test4 = TestCase (assertEqual "maybeAppend supports constructor Nothing."
                              (Nothing :: Maybe [Int])
                              (maybeAppend 5 Nothing))

test5 = TestCase (assertEqual "maybeAppend supports constructor Just (type Int)."
                              (Just [5, 7] :: Maybe [Int])
                              (maybeAppend 5 (Just [7] :: Maybe [Int])))

test6 = TestCase (assertEqual "maybeAppend supports constructor Just (type String)."
                              (Just ["Hello", "World"] :: Maybe [String])
                              (maybeAppend "Hello" (Just ["World"] :: Maybe [String])))

-----------------------------------------------------------------------------------------
-- setMaybe

test7 = TestCase (assertEqual "setMaybe supports constructor Nothing (1/2)."
                              (Just 5)
                              (setMaybe 5 Nothing))

test8 = TestCase (assertEqual "setMaybe supports constructor Nothing (2/2)."
                              (Just "Hello")
                              (setMaybe "Hello" Nothing))

test9 = TestCase (assertEqual "setMaybe supports constructor Just (1/2)."
                              (Just 42)
                              (setMaybe 5 (Just 42)))

test10 = TestCase (assertEqual "setMaybe supports constructor Just (2/2)."
                               (Just "Goodbye")
                               (setMaybe "Hello" (Just "Goodbye")))

-----------------------------------------------------------------------------------------
-- branchJust

test11 = TestCase (assertEqual "branchJust supports constructor Nothing."
                               (Nothing :: Maybe Int)
                               (branchJust Nothing id))

test12 = TestCase (assertEqual "branchJust supports constructor Just (type Int)."
                               (Just 7 :: Maybe Int)
                               (branchJust (Just 5 :: Maybe Int) f))
    where f x = Just $ x + 2

test13 = TestCase (assertEqual "branchJust supports constructor Just (type String)."
                               (Just "Hello World" :: Maybe String)
                               (branchJust (Just "Hello" :: Maybe String) f))
    where f str = Just $ str ++ " World"

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "maybeWrap_Nothing" test1,
                                     TestLabel "maybeWrap_JustInt" test2,
                                     TestLabel "maybeWrap_JustStr" test3,
                                     TestLabel "maybeAppend_Nothing" test4,
                                     TestLabel "maybeAppend_JustInt" test5,
                                     TestLabel "maybeAppend_JustStr" test6,
                                     TestLabel "setMaybe_NothingInt" test7,
                                     TestLabel "setMaybe_NothingStr" test8,
                                     TestLabel "setMaybe_JustInt" test9,
                                     TestLabel "setMaybe_JustStr" test10,
                                     TestLabel "branchJust_Nothing" test11,
                                     TestLabel "branchJust_JustInt" test12,
                                     TestLabel "branchJust_JustStr" test13]

main = defaultMain tests
