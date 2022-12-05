-- | Simple unit tests for the Quipper parser. Ensures that a valid file can be read,
-- and that at least one superficial property of the file is parsed correctly.

module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Quip.Gate
import Quip.GateName
import Quip.Parser

-----------------------------------------------------------------------------------------
-- parseQuip

test1 = TestCase (assertEqual "parseQuip supports valid files."
                             2
                             (length shape))
    where input = "Inputs: 0:Qbit, 1:Qbit\n" ++
                  "QGate[\"not\"](0) with controls=[+1]\n" ++
                  "Outputs: 0:Qbit, 1:Qbit"
          qcirc = parseQuip "file" input
          QuipCirc _ shape = qcirc

-----------------------------------------------------------------------------------------
-- quipToCirc

checkGate0 :: Gate -> Bool
checkGate0 (NamedGate GateX False [0] [Pos 1]) = True
checkGate0 _                                   = False

checkGate1 :: Gate -> Bool
checkGate1 (NamedGate GateY True [1] [Neg 0]) = True
checkGate1 _                                  = False

checkGate2 :: Gate -> Bool
checkGate2 (RotGate RotExpZ False 1 [0] []) = True
checkGate2 _                                = False

checkGate3 :: Gate -> Bool
checkGate3 (PhaseGate 2 []) = True
checkGate3 _                = False

test2 = TestCase (assertBool "quipToGates supports valid file with unitary gates."
                             (and [length qgate == 4,
                                   checkGate0 (qgate !! 0),
                                   checkGate1 (qgate !! 1),
                                   checkGate2 (qgate !! 2),
                                   checkGate3 (qgate !! 3)]))
    where input = "Inputs: 0:Qbit, 1:Qbit\n" ++
                  "QGate[\"not\"](0) with controls=[+1]\n" ++
                  "QGate[\"Y\"]*(1) with controls=[-0]\n" ++
                  "QRot[\"exp(-i%Z)\",1](0)\n" ++
                  "GPhase() with t=2\n" ++
                  "Outputs: 0:Qbit, 1:Qbit"
          qcirc = parseQuip "file" input
          gcirc = quipToGates qcirc
          qgate = gates gcirc

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "parseQuip" test1,
                                     TestLabel "quipToCirc" test2]

main = defaultMain tests
