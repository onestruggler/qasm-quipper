-- | Simple unit tests for the Quipper parser. Ensures that a valid file can be read,
-- and that at least one superficial property of the file is parsed correctly.

module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Quip.Parser

-----------------------------------------------------------------------------------------
-- Test Cases

runTest :: Bool -> String -> Bool
runTest isValid input = 0 < (length $ shape $ circ)
    where circ = parseQuip "file" input

test1 = TestCase (assertEqual "parseQuip supports valid files."
                             2
                             (length $ shape $ qcirc))
    where input = "Inputs: 0:Qbit, 1:Qbit\n" ++
                  "QGate[\"not\"](0) with controls=[+1]\n" ++
                  "Outputs: 0:Qbit, 1:Qbit"
          qcirc = parseQuip "file" input

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "ValidFile" test1]

main = defaultMain tests
