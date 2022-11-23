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
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "maybeWrap_Nothing" test1,
                                     TestLabel "maybeWrap_JustInt" test2,
                                     TestLabel "maybeWrap_JustStr" test3,
                                     TestLabel "maybeAppend_Nothing" test4,
                                     TestLabel "maybeAppend_JustInt" test5,
                                     TestLabel "maybeAppend_JustStr" test6]

main = defaultMain tests
