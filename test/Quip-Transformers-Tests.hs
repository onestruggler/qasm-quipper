-- | Simple unit tests for Quipper transformers. End-to-end testing of the
-- pipeline is left to integration tests. The purpose of this test suite is to
-- ensure that Qasm.Transformers does not break on simple use-cases.

{-# LANGUAGE Rank2Types #-}

module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Quip.Gate
import Quip.GateName
import Quip.Parser
import Quip.Transformers
import Quipper

-----------------------------------------------------------------------------------------
-- Helper method to test on plain text.

apply :: Transformer Circ Qubit Bit -> String -> [Gate]
apply t = gates . quipToGates . applyTransformer t . parseQuip "x"

-----------------------------------------------------------------------------------------
-- elimCtrlsTransformer

-- Simple Gates (Few Controls): Gates with controlled versions in OpenQasm.
ascii_not   = "QGate[\"not\"](0)"
ascii_cnot  = "QGate[\"not\"](0) with controls=[+1]"
ascii_ccnot = "QGate[\"not\"](0) with controls=[+1, +2]"
ascii_x     = "QGate[\"X\"](0)"
ascii_cx    = "QGate[\"X\"](0) with controls=[+1]"
ascii_ccx   = "QGate[\"X\"](0) with controls=[+1, +2]"
ascii_y     = "QGate[\"Y\"](0)"
ascii_cy    = "QGate[\"Y\"](0) with controls=[+1]"
ascii_z     = "QGate[\"Z\"](0)"
ascii_cz    = "QGate[\"Z\"](0) with controls=[+1]"
ascii_h     = "QGate[\"H\"](0)"
ascii_ch    = "QGate[\"H\"](0) with controls=[+1]"
ascii_swp   = "QGate[\"swap\"](0, 1)"
ascii_cswp  = "QGate[\"swap\"](0, 1) with controls=[+1]"

abs_x    = NamedGate GateX False [0] []
abs_cx   = NamedGate GateX False [0] [Pos 1]
abs_ccx  = NamedGate GateX False [0] [Pos 1, Pos 2]
abs_y    = NamedGate GateY False [0] []
abs_cy   = NamedGate GateY False [0] [Pos 1]
abs_z    = NamedGate GateZ False [0] []
abs_cz   = NamedGate GateZ False [0] [Pos 1]
abs_h    = NamedGate GateH False [0] []
abs_ch   = NamedGate GateH False [0] [Pos 1]
abs_swp  = NamedGate GateSwap False [0, 1] []
abs_cswp = NamedGate GateSwap False [0, 1] [Pos 1]

test1 = TestCase (assertEqual "elimCtrlsTransformer on simple gates with few ctrls."
                              output
                              (apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit\n" ++
                   ascii_not ++ "\n" ++ ascii_cnot ++ "\n" ++ ascii_ccnot ++ "\n" ++
                   ascii_x   ++ "\n" ++ ascii_cx   ++ "\n" ++ ascii_ccx   ++ "\n" ++
                   ascii_y   ++ "\n" ++ ascii_cy   ++ "\n" ++
                   ascii_z   ++ "\n" ++ ascii_cz   ++ "\n" ++
                   ascii_h   ++ "\n" ++ ascii_ch   ++ "\n" ++
                   ascii_swp ++ "\n" ++ ascii_cswp ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit"
          output = [abs_x, abs_cx, abs_ccx,
                    abs_x, abs_cx, abs_ccx,
                    abs_y, abs_cy,
                    abs_z, abs_cz,
                    abs_h, abs_ch,
                    abs_swp, abs_cswp]

-- Simple Gates (Several Controls)
ascii_cccnot = "QGate[\"not\"](0) with controls=[+1, +2, +3]"
ascii_cccx   = "QGate[\"X\"](0) with controls=[+1, +2, +3]"
ascii_ccy    = "QGate[\"Y\"](0) with controls=[+1, +2]"
ascii_ccz    = "QGate[\"Z\"](0) with controls=[+1, +2]"
ascii_cch    = "QGate[\"H\"](0) with controls=[+1, +2]"
ascii_ccswp  = "QGate[\"swap\"](0, 1) with controls=[+1, +2]"

test2 = TestCase (assertEqual "elimCtrlsTransformer on CCC(not)."
                              5
                              (length $ apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_cccnot ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test3 = TestCase (assertEqual "elimCtrlsTransformer on CCC(X)."
                              5
                              (length $ apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_cccx ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test4 = TestCase (assertEqual "elimCtrlsTransformer on CC(Y)."
                              5
                              (length $ apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_ccy ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test5 = TestCase (assertEqual "elimCtrlsTransformer on CC(Z)."
                              5
                              (length $ apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_ccz ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test6 = TestCase (assertEqual "elimCtrlsTransformer on CC(H)."
                              5
                              (length $ apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_cch ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test7 = TestCase (assertEqual "elimCtrlsTransformer on CC(swap)."
                              5
                              (length $ apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_ccswp ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

-- Simple Gates (Many Controls)
ascii_cccz = "QGate[\"Z\"](0) with controls=[+1, +2, +3]"

test8 = TestCase (assertEqual "elimCtrlsTransformer on CCC(Z)."
                              9
                              (length $ apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_cccz ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

-- Control Gates
ascii_gphase    = "GPhase() with t=1"
ascii_cgphase   = "GPhase() with t=2 with controls=[+1]"
ascii_ccgphase  = "GPhase() with t=1.5 with controls=[+1, +2]"
ascii_cccgphase = "GPhase() with t=3 with controls=[+1, +2, +3]"

abs_gphase  = PhaseGate 1.0 []
abs_cgphase = PhaseGate 2.0 [Pos 1]

test9 = TestCase (assertEqual "elimCtrlsTransformer on GPhase()."
                              [abs_gphase]
                              (apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_gphase ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test10 = TestCase (assertEqual "elimCtrlsTransformer on C(GPhase())."
                               [abs_cgphase]
                               (apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_cgphase ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test11 = TestCase (assertEqual "elimCtrlsTransformer on CC(GPhase())."
                               5
                               (length $ apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_ccgphase ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test12 = TestCase (assertEqual "elimCtrlsTransformer on CCC(GPhase())."
                               9
                               (length $ apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_cccgphase ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "elimCtrlsTransformer_QGate_1" test1,
                                     TestLabel "elimCtrlsTransformer_QGate_2" test2,
                                     TestLabel "elimCtrlsTransformer_QGate_3" test3,
                                     TestLabel "elimCtrlsTransformer_QGate_4" test4,
                                     TestLabel "elimCtrlsTransformer_QGate_5" test5,
                                     TestLabel "elimCtrlsTransformer_QGate_6" test6,
                                     TestLabel "elimCtrlsTransformer_QGate_7" test7,
                                     TestLabel "elimCtrlsTransformer_QGate_8" test8,
                                     TestLabel "elimCtrlsTransformer_QGate_9" test9,
                                     TestLabel "elimCtrlsTransformer_QGate_10" test10,
                                     TestLabel "elimCtrlsTransformer_QGate_11" test11,
                                     TestLabel "elimCtrlsTransformer_QGate_12" test12]

main = defaultMain tests
