-- | Simple unit tests for the OpenQASM parser. Ensures that both valid and
-- invalid OpenQASM files are handled correctly.

module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.Language
import LinguaQuanta.Qasm.Parser

-----------------------------------------------------------------------------------------
-- Test Cases

runTest :: Bool -> String -> Bool
runTest isValid input =
    case parseQasm "file" input of
        Right _ -> not isValid
        Left _  -> isValid

test1 = TestCase (assertBool "parseQasm supports valid files."
                             (runTest True input))
    where input = "inv @ pow(2) @ cx v1, v2;"

test2 = TestCase (assertBool "parseQasm supports invalid files."
                             (runTest False input))
    where input = "inv @ @ pow(2) @ cx v1, v2;"

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "ValidFile" test1,
                                     TestLabel "InvalidFile" test2]

main = defaultMain tests
