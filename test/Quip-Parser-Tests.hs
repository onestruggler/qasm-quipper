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
-- Testing Baseline

asciiGateX = "QGate[\"X\"](0) with controls=[+1]"
asciiGate0 = "QGate[\"not\"](0) with controls=[+1]"
asciiGate1 = "QGate[\"iX\"]*(1) with controls=[-0]"
asciiGate2 = "QRot[\"exp(-i%Z)\",1.0](0)"
asciiGate3 = "GPhase() with t=2.0"

absGate0 = NamedGate GateX False [0] [Pos 1]
absGate1 = NamedGate GateIX True [1] [Neg 0]
absGate2 = RotGate RotExpZ False 1 [0] []
absGate3 = PhaseGate 2 []

-----------------------------------------------------------------------------------------
-- parseQuip

test1 = TestCase (assertEqual "parseQuip supports valid files."
                              2
                              (length shape))
    where input = "Inputs: 0:Qbit, 1:Qbit\n" ++
                  asciiGate0 ++
                  "Outputs: 0:Qbit, 1:Qbit"
          qcirc = parseQuip "file" input
          QuipCirc _ shape = qcirc

-----------------------------------------------------------------------------------------
-- quipToCirc

simpleInput = "Inputs: 0:Qbit, 1:Qbit\n" ++
               asciiGateX ++ "\n" ++
               asciiGate1 ++ "\n" ++
               asciiGate2 ++ "\n" ++
               asciiGate3 ++ "\n" ++
               "Outputs: 0:Qbit, 1:Qbit"

test2 = TestCase (assertBool "quipToGates supports valid file with unitary gates."
                             (and [length qgates == 4,
                                   qgates !! 0 == absGate0,
                                   qgates !! 1 == absGate1,
                                   qgates !! 2 == absGate2,
                                   qgates !! 3 == absGate3]))
    where qcirc  = parseQuip "file" simpleInput
          gcirc  = quipToGates qcirc
          qgates = gates gcirc

-----------------------------------------------------------------------------------------
-- gatesToAscii

test3 = TestCase (assertEqual "gatesToAscii supports 1-to-1 mapping of unitary gates."
                              (simpleInput ++ "\n\n")
                              (gatesToAscii $ quipToGates $ parseQuip "x" simpleInput))

test4 = TestCase (assertEqual "gatesToAscii supports multiple controls on unitaries."
                              (input ++ "\n\n")
                              (gatesToAscii $ quipToGates $ parseQuip "x" input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit\n" ++
                  "QGate[\"iX\"]*(2) with controls=[-0, +1]\n" ++
                  "QRot[\"exp(-i%Z)\",1.0]*(0) with controls=[-0, +1]\n" ++
                  "GPhase() with t=2.0 with controls=[-0, +1]\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit"

test5 = TestCase (assertEqual "gatesToAscii supports multiple controls on unitaries."
                              (output ++ "\n\n")
                              (gatesToAscii $ quipToGates $ parseQuip "x" input))
    where input  = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit\n" ++
                   "QGate[\"MyG\"]*(2); [1] with controls=[-0]\n" ++
                   "Outputs: 0:Qbit, 1:Qbit, 2:Qbit"
          output = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit\n" ++
                   "QGate[\"MyG\"]*(1,2) with controls=[-0]\n" ++
                   "Outputs: 0:Qbit, 1:Qbit, 2:Qbit"

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "parseQuip" test1,
                                     TestLabel "quipToCirc" test2,
                                     TestLabel "gatesToAscii_Basic" test3,
                                     TestLabel "gatesToAscii_ManyCtrl" test4,
                                     TestLabel "gatesToAscii_GeneralCtrls" test5]

main = defaultMain tests
