module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.AST
import LinguaQuanta.QasmToQuip.Translator
import LinguaQuanta.Quip.Quipper
import LinguaQuanta.Quip.Wire

import qualified Data.IntMap.Strict as IntMap

-----------------------------------------------------------------------------------------
-- Basic QWire Tests.

circ1 = translate []
circ2 = translate [AstQubitDecl Nothing  "decl1"]
circ3 = translate [AstQubitDecl (Just 4) "decl1"]
circ4 = translate [AstQubitDecl (Just 4) "decl1", AstQubitDecl (Nothing) "decl2"]

test1 = TestCase (assertEqual "translate (inputs): empty circuit."
                              IntMap.empty
                              (outputs circ1))

test2 = TestCase (assertEqual "translate (gates): empty circuit."
                              []
                              (gates circ1))

test3 = TestCase (assertEqual "translate (outputs): empty circuit."
                              IntMap.empty
                              (outputs circ1))

test4 = TestCase (assertEqual "translate (size): empty circuit."
                              0
                              (size circ1))

circ2_io = IntMap.fromList [(0, QWire)]

test5 = TestCase (assertEqual "translate (inputs): single qbit."
                              circ2_io
                              (outputs circ2))

test6 = TestCase (assertEqual "translate (gates): single qbit."
                              []
                              (gates circ2))

test7 = TestCase (assertEqual "translate (outputs): single qbit."
                              circ2_io
                              (outputs circ2))

test8 = TestCase (assertEqual "translate (size): single qbit."
                              1
                              (size circ2))

circ3_io = IntMap.fromList [(0, QWire), (1, QWire), (2, QWire), (3, QWire)]

test9 = TestCase (assertEqual "translate (inputs): qbit array."
                              circ3_io
                              (outputs circ3))

test10 = TestCase (assertEqual "translate (gates): qbit array."
                               []
                               (gates circ3))

test11 = TestCase (assertEqual "translate (outputs): qbit array."
                               circ3_io
                               (outputs circ3))

test12 = TestCase (assertEqual "translate (size): qbit array."
                               4
                               (size circ3))

circ4_io = IntMap.fromList [(0, QWire), (1, QWire), (2, QWire), (3, QWire), (4, QWire)]

test13 = TestCase (assertEqual "translate (inputs): qbit array and single qubit."
                               circ4_io
                               (outputs circ4))

test14 = TestCase (assertEqual "translate (gates): qbit array and single qubit."
                               []
                               (gates circ4))

test15 = TestCase (assertEqual "translate (outputs): qbit array and single qubit."
                               circ4_io
                               (outputs circ4))

test16 = TestCase (assertEqual "translate (size): qbit array and single qubit."
                               5
                               (size circ4))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "Empty_Inputs" test1,
                                     TestLabel "Empty_Gates" test2,
                                     TestLabel "Empty_Outputs" test3,
                                     TestLabel "Empty_Size" test4,
                                     TestLabel "1Qbit_Inputs" test5,
                                     TestLabel "1Qbit_Gates" test6,
                                     TestLabel "1Qbit_Outputs" test7,
                                     TestLabel "1Qbit_Size" test8,
                                     TestLabel "1Qarr_Inputs" test9,
                                     TestLabel "1Qarr_Gates" test10,
                                     TestLabel "1Qarr_Outputs" test11,
                                     TestLabel "1Qarr_Size" test12,
                                     TestLabel "1Qarr1Qbit_Inputs" test13,
                                     TestLabel "1Qarr1Qbit_Gates" test14,
                                     TestLabel "1Qarr1Qbit_Outputs" test15,
                                     TestLabel "1Qarr1Qbit_Size" test16]

main = defaultMain tests
