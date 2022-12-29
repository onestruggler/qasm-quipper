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
ascii_not    = "QGate[\"not\"](0)"
ascii_cnot   = "QGate[\"not\"](0) with controls=[+1]"
ascii_ccnot  = "QGate[\"not\"](0) with controls=[+1, +2]"
ascii_x      = "QGate[\"X\"](0)"
ascii_cx     = "QGate[\"X\"](0) with controls=[+1]"
ascii_ccx    = "QGate[\"X\"](0) with controls=[+1, +2]"
ascii_y      = "QGate[\"Y\"](0)"
ascii_cy     = "QGate[\"Y\"](0) with controls=[+1]"
ascii_z      = "QGate[\"Z\"](0)"
ascii_cz     = "QGate[\"Z\"](0) with controls=[+1]"
ascii_h      = "QGate[\"H\"](0)"
ascii_ch     = "QGate[\"H\"](0) with controls=[+1]"
ascii_swp    = "QGate[\"swap\"](0, 1)"
ascii_cswp   = "QGate[\"swap\"](0, 1) with controls=[+1]"
ascii_mnot   = "QGate[\"multinot\"](0,1)"
ascii_cmnot  = "QGate[\"multinot\"](0,1) with controls=[+2]"
ascii_ccmnot = "QGate[\"multinot\"](0,1) with controls=[+2, +3]"

abs_x      = NamedGate GateX False [0] []
abs_cx     = NamedGate GateX False [0] [Pos 1]
abs_ccx    = NamedGate GateX False [0] [Pos 1, Pos 2]
abs_y      = NamedGate GateY False [0] []
abs_cy     = NamedGate GateY False [0] [Pos 1]
abs_z      = NamedGate GateZ False [0] []
abs_cz     = NamedGate GateZ False [0] [Pos 1]
abs_h      = NamedGate GateH False [0] []
abs_ch     = NamedGate GateH False [0] [Pos 1]
abs_swp    = NamedGate GateSwap False [0, 1] []
abs_cswp   = NamedGate GateSwap False [0, 1] [Pos 1]
abs_mnot   = NamedGate GateQMultiNot False [0, 1] []
abs_cmnot  = NamedGate GateQMultiNot False [0, 1] [Pos 2]
abs_ccmnot = NamedGate GateQMultiNot False [0, 1] [Pos 2, Pos 3]

test1 = TestCase (assertEqual "elimCtrlsTransformer on simple gates with few ctrls."
                              output
                              (apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                   ascii_not  ++ "\n" ++ ascii_cnot  ++ "\n" ++ ascii_ccnot  ++ "\n" ++
                   ascii_x    ++ "\n" ++ ascii_cx    ++ "\n" ++ ascii_ccx    ++ "\n" ++
                   ascii_y    ++ "\n" ++ ascii_cy    ++ "\n" ++
                   ascii_z    ++ "\n" ++ ascii_cz    ++ "\n" ++
                   ascii_h    ++ "\n" ++ ascii_ch    ++ "\n" ++
                   ascii_swp  ++ "\n" ++ ascii_cswp  ++ "\n" ++
                   ascii_mnot ++ "\n" ++ ascii_cmnot ++ "\n" ++ ascii_ccmnot ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"
          output = [abs_x, abs_cx, abs_ccx,
                    abs_x, abs_cx, abs_ccx,
                    abs_y, abs_cy,
                    abs_z, abs_cz,
                    abs_h, abs_ch,
                    abs_swp, abs_cswp,
                    abs_mnot, abs_cmnot, abs_ccmnot]

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

-- Phase Gates
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

-- Rotational Gates
ascii_rot    = "QRot[\"exp(-i%Z)\",0.5](0)"
ascii_crot   = "QRot[\"exp(-i%Z)\",1](0) with controls=[+1]"
ascii_ccrot  = "QRot[\"exp(-i%Z)\",2](0) with controls=[+1, +2]"
ascii_cccrot = "QRot[\"exp(-i%Z)\",3](0) with controls=[+1, +2, +3]"

abs_rot = RotGate RotExpZ False 0.5 [0] []

test13 = TestCase (assertEqual "elimCtrlsTransformer on expZt."
                               [abs_rot]
                               (apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_rot ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test14 = TestCase (assertEqual "elimCtrlsTransformer on C(expZt)."
                               4
                               (length $ apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_crot ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test15 = TestCase (assertEqual "elimCtrlsTransformer on CC(expZt)."
                               8
                               (length $ apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_ccrot ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test16 = TestCase (assertEqual "elimCtrlsTransformer on CCC(expZt)."
                               12
                               (length $ apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_cccrot ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

-- Generalized Controlled QGates
ascii_gctrl = "QGate[\"MyGate\"](0,1); [2,3]"

abs_gctrl = NamedGate (UserDefinedGate "MyGate") False [2, 3, 0, 1] []

test17 = TestCase (assertEqual "elimCtrlsTransformer on QGate with generalized controls."
                               [abs_gctrl]
                               (apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_gctrl ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

-- Omega QGate
ascii_omega    = "QGate[\"omega\"](0)"
ascii_comega   = "QGate[\"omega\"](0) with controls=[+1]"
ascii_ccomega  = "QGate[\"omega\"](0) with controls=[+1, +2]"
ascii_cccomega = "QGate[\"omega\"](0) with controls=[+1, +2, +3]"

abs_omega  = NamedGate GateOmega False [0] []
abs_comega = PhaseGate 0.25 [Pos 1]

test18 = TestCase (assertEqual "elimCtrlsTransformer on omega."
                               [abs_omega]
                               (apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_omega ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test19 = TestCase (assertEqual "elimCtrlsTransformer on C(omega)."
                               [abs_comega]
                               (apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_comega ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test20 = TestCase (assertEqual "elimCtrlsTransformer on CC(omega)."
                               5
                               (length $ apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_ccomega ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test21 = TestCase (assertEqual "elimCtrlsTransformer on CCC(omega)."
                               9
                               (length $ apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_cccomega ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

-- W QGate
ascii_w    = "QGate[\"W\"](0,1)"
ascii_cw   = "QGate[\"W\"](0,1) with controls=[+2]"
ascii_ccw  = "QGate[\"W\"](0,1) with controls=[+2, +3]"
ascii_cccw = "QGate[\"W\"](0,1) with controls=[+2, +3, +4]"

abs_w  = NamedGate GateW False [0, 1] []

test22 = TestCase (assertEqual "elimCtrlsTransformer on W."
                               [abs_w]
                               (apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit\n" ++
                  ascii_w ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit"

test23 = TestCase (assertEqual "elimCtrlsTransformer on C(W)."
                               output
                               (apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit\n" ++
                  ascii_cw ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit"
          output = [NamedGate GateX False [1] [Pos 0],
                    NamedGate GateS True  [0] [],
                    NamedGate GateH False [0] [],
                    NamedGate GateT True  [0] [],
                    NamedGate GateH False [0] [],
                    NamedGate GateT False [0] [],
                    NamedGate GateT False [1] [],
                    NamedGate GateT False [2] [],
                    NamedGate GateX False [2] [Pos 1],
                    NamedGate GateX False [1] [Pos 0],
                    NamedGate GateX False [0] [Pos 2],
                    NamedGate GateT True  [1] [],
                    NamedGate GateT False [0] [],
                    NamedGate GateX False [1] [Pos 2],
                    NamedGate GateT True  [1] [],
                    NamedGate GateT True  [2] [],
                    NamedGate GateX False [1] [Pos 0],
                    NamedGate GateX False [0] [Pos 2],
                    NamedGate GateX False [2] [Pos 1],
                    NamedGate GateH False [0] [],
                    NamedGate GateT False [0] [],
                    NamedGate GateH False [0] [],
                    NamedGate GateS False [0] [],
                    NamedGate GateX False [1] [Pos 0]]

test24 = TestCase (assertEqual "elimCtrlsTransformer on CC(W)."
                               28
                               (length $ apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit\n" ++
                  ascii_ccw ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit"

test25 = TestCase (assertEqual "elimCtrlsTransformer on CCC(W)."
                               32
                               (length $ apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit\n" ++
                  ascii_cccw ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit"

-- Special Gates (No Controls)
ascii_e  = "QGate[\"E\"](0)"
ascii_ix = "QGate[\"iX\"](0)"
ascii_s  = "QGate[\"S\"](0)"
ascii_t  = "QGate[\"T\"](0)"
ascii_v  = "QGate[\"V\"](0)"

abs_e  = NamedGate GateE False  [0] []
abs_ix = NamedGate GateIX False [0] []
abs_s  = NamedGate GateS False  [0] []
abs_t  = NamedGate GateT False  [0] []
abs_v  = NamedGate GateV False  [0] []

test26 = TestCase (assertEqual "elimCtrlsTransformer on controll-free special gates."
                               output
                               (apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                   ascii_e  ++ "\n" ++
                   ascii_ix ++ "\n" ++
                   ascii_s  ++ "\n" ++
                   ascii_t  ++ "\n" ++
                   ascii_v  ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"
          output = [abs_e, abs_ix, abs_s, abs_t, abs_v]

-- Special Gates (Single Control)
ascii_ce  = "QGate[\"E\"](0) with controls=[+1]"
ascii_cix = "QGate[\"iX\"](0) with controls=[+1]"
ascii_cs  = "QGate[\"S\"](0) with controls=[+1]"
ascii_ct  = "QGate[\"T\"](0) with controls=[+1]"
ascii_cv  = "QGate[\"V\"](0) with controls=[+1]"

test27 = TestCase (assertEqual "elimCtrlsTransformer on C(E)."
                               output
                               (apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                   ascii_ce ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"
          output = [NamedGate GateH False [0] [],
                    NamedGate GateS False [1] [],
                    NamedGate GateT False [0] [],
                    NamedGate GateX False [0] [Pos 1],
                    NamedGate GateT True  [0] [],
                    NamedGate GateH False [0] [],
                    NamedGate GateX False [1] [Pos 0],
                    NamedGate GateT False [1] [],
                    NamedGate GateT True  [0] [],
                    NamedGate GateX False [1] [Pos 0]]

test28 = TestCase (assertEqual "elimCtrlsTransformer on C(iX)."
                               output
                               (apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                   ascii_cix ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"
          output = [NamedGate GateX False [0] [Pos 1],
                    NamedGate GateS False [1] []]

test29 = TestCase (assertEqual "elimCtrlsTransformer on C(S)."
                               output
                               (apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                   ascii_cs ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"
          output = [NamedGate GateX False [1] [Pos 0],
                    NamedGate GateT True  [1] [],
                    NamedGate GateX False [1] [Pos 0],
                    NamedGate GateT False [1] [],
                    NamedGate GateT False [0] []]

test30 = TestCase (assertEqual "elimCtrlsTransformer on C(T)."
                               output
                               (apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                   ascii_ct ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"
          output = [QInitGate False 4,
                    NamedGate GateH False [4] [],
                    NamedGate GateX False [0] [Pos 4],
                    NamedGate GateX False [1] [Pos 0],
                    NamedGate GateT False [0] [],
                    NamedGate GateT True  [1] [],
                    NamedGate GateX False [0] [Pos 4],
                    NamedGate GateX False [1] [Pos 0],
                    NamedGate GateT True  [4] [],
                    NamedGate GateT False [1] [],
                    NamedGate GateX False [1] [Pos 4],
                    NamedGate GateH False [4] [],
                    NamedGate GateT False [4] [],
                    NamedGate GateH True  [4] [],
                    NamedGate GateX True  [1] [Pos 4],
                    NamedGate GateT True  [1] [],
                    NamedGate GateT False [4] [],
                    NamedGate GateX True  [1] [Pos 0],
                    NamedGate GateX True  [0] [Pos 4],
                    NamedGate GateT False [1] [],
                    NamedGate GateT True  [0] [],
                    NamedGate GateX True  [1] [Pos 0],
                    NamedGate GateX True  [0] [Pos 4],
                    NamedGate GateH True  [4] [],
                    QTermGate False 4]

test31 = TestCase (assertEqual "elimCtrlsTransformer on C(V)."
                               output
                               (apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                   ascii_cv ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"
          output = [NamedGate GateH False [0] [],
                    NamedGate GateT True  [1] [],
                    NamedGate GateX False [1] [Pos 0],
                    NamedGate GateT False [1] [],
                    NamedGate GateT True  [0] [],
                    NamedGate GateX False [1] [Pos 0],
                    NamedGate GateH False [0] []]

-- Special Gates (Many Controls)
ascii_cce  = "QGate[\"E\"](0) with controls=[+1, +2]"
ascii_ccce = "QGate[\"E\"](0) with controls=[+1, +2, +3]"

test32 = TestCase (assertEqual "elimCtrlsTransformer on CC(E)."
                               14
                               (length $ apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit\n" ++
                  ascii_cce ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit"

test33 = TestCase (assertEqual "elimCtrlsTransformer on CCC(E)."
                               18
                               (length $ apply elimCtrlsTransformer input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit\n" ++
                  ascii_ccce ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit"

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
                                     TestLabel "elimCtrlsTransformer_QGate_12" test12,
                                     TestLabel "elimCtrlsTransformer_QGate_13" test13,
                                     TestLabel "elimCtrlsTransformer_QGate_14" test14,
                                     TestLabel "elimCtrlsTransformer_QGate_15" test15,
                                     TestLabel "elimCtrlsTransformer_QGate_16" test16,
                                     TestLabel "elimCtrlsTransformer_QGate_17" test17,
                                     TestLabel "elimCtrlsTransformer_QGate_18" test18,
                                     TestLabel "elimCtrlsTransformer_QGate_19" test19,
                                     TestLabel "elimCtrlsTransformer_QGate_20" test20,
                                     TestLabel "elimCtrlsTransformer_QGate_21" test21,
                                     TestLabel "elimCtrlsTransformer_QGate_22" test22,
                                     TestLabel "elimCtrlsTransformer_QGate_23" test23,
                                     TestLabel "elimCtrlsTransformer_QGate_24" test24,
                                     TestLabel "elimCtrlsTransformer_QGate_25" test25,
                                     TestLabel "elimCtrlsTransformer_QGate_26" test26,
                                     TestLabel "elimCtrlsTransformer_QGate_27" test27,
                                     TestLabel "elimCtrlsTransformer_QGate_28" test28,
                                     TestLabel "elimCtrlsTransformer_QGate_29" test29,
                                     TestLabel "elimCtrlsTransformer_QGate_30" test30,
                                     TestLabel "elimCtrlsTransformer_QGate_31" test31,
                                     TestLabel "elimCtrlsTransformer_QGate_32" test32,
                                     TestLabel "elimCtrlsTransformer_QGate_33" test33]

main = defaultMain tests
