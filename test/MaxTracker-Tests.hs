module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.MaxTracker

-----------------------------------------------------------------------------------------
-- Integer Test.

maxint0 = initTracker 5
maxint1 = updateTracker maxint0 $ \x -> x + 5
maxint2 = updateTracker maxint1 $ \_ -> 0
maxint3 = updateTracker maxint2 $ \x -> (x - 1) * (-20)
maxint4 = updateTracker maxint3 $ \_ -> 10

test1 = TestCase (assertEqual "Inspecting maxima along (MaxTracker Int) trace (1/5)."
                               5
                               (maxval maxint0))

test2 = TestCase (assertEqual "Inspecting maxima along (MaxTracker Int) trace (2/5)."
                               10
                               (maxval maxint1))

test3 = TestCase (assertEqual "Inspecting maxima along (MaxTracker Int) trace (3/5)."
                               10
                               (maxval maxint2))

test4 = TestCase (assertEqual "Inspecting maxima along (MaxTracker Int) trace (4/5)."
                               20
                               (maxval maxint3))

test5 = TestCase (assertEqual "Inspecting maxima along (MaxTracker Int) trace (5/5)."
                               20
                               (maxval maxint4))

test6 = TestCase (assertEqual "Inspecting values along (MaxTracker Int) trace (1/5)."
                               5
                               (curval maxint0))

test7 = TestCase (assertEqual "Inspecting values along (MaxTracker Int) trace (2/5)."
                               10
                               (curval maxint1))

test8 = TestCase (assertEqual "Inspecting values along (MaxTracker Int) trace (3/5)."
                               0
                               (curval maxint2))

test9 = TestCase (assertEqual "Inspecting values along (MaxTracker Int) trace (4/5)."
                               20
                               (curval maxint3))

test10 = TestCase (assertEqual "Inspecting values along (MaxTracker Int) trace (5/5)."
                                10
                                (curval maxint4))

-----------------------------------------------------------------------------------------
-- Character Test.

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "MaxTracker_Int_maxval_1" test1,
                                     TestLabel "MaxTracker_Int_maxval_2" test2,
                                     TestLabel "MaxTracker_Int_maxval_3" test3,
                                     TestLabel "MaxTracker_Int_maxval_4" test4,
                                     TestLabel "MaxTracker_Int_maxval_5" test5,
                                     TestLabel "MaxTracker_Int_curval_1" test6,
                                     TestLabel "MaxTracker_Int_curval_2" test7,
                                     TestLabel "MaxTracker_Int_curval_3" test8,
                                     TestLabel "MaxTracker_Int_curval_4" test9,
                                     TestLabel "MaxTracker_Int_curval_5" test10]

main = defaultMain tests
