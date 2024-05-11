-- | Simple unit tests for Quipper transformers. End-to-end testing of the
-- pipeline is left to integration tests. The purpose of this test suite is to
-- ensure that Qasm.Transformers does not break on simple use-cases.

{-# LANGUAGE Rank2Types #-}

module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Quip.Gate
import LinguaQuanta.Quip.GateName
import LinguaQuanta.Quip.Quipper
import LinguaQuanta.Quip.Transformers
import Quipper

-----------------------------------------------------------------------------------------
-- Helper method to test on plain text.

apply :: Transformer Circ Qubit Bit -> String -> [Gate]
apply t = gates . quipToGates . applyTransformer t . parseQuip "x"

elimWithTof = elimCtrlsTransformer $ ElimCtrlsConf { tofRule   = UseTof
                                                   , elimCH    = False
                                                   , elimCSwap = False
                                                   , druleMap  = emptyDruleMap
                                                   }

elimWithoutTof = elimCtrlsTransformer $ ElimCtrlsConf { tofRule   = ElimTof
                                                      , elimCH    = False
                                                      , elimCSwap = False
                                                      , druleMap  = emptyDruleMap
                                                      }

elimWithoutFredkin = elimCtrlsTransformer $ ElimCtrlsConf { tofRule   = UseTof
                                                          , elimCH    = False
                                                          , elimCSwap = True
                                                          , druleMap  = emptyDruleMap
                                                          }

elimWithoutCH = elimCtrlsTransformer $ ElimCtrlsConf { tofRule   = UseTof
                                                     , elimCH    = True
                                                     , elimCSwap = False
                                                     , druleMap  = emptyDruleMap
                                                     }

elimMaxInline = elimCtrlsTransformer $ ElimCtrlsConf { tofRule   = ElimTof
                                                     , elimCH    = True
                                                     , elimCSwap = True
                                                     , druleMap  = emptyDruleMap
                                                     }

elimUseCCIX = elimCtrlsTransformer $ ElimCtrlsConf { tofRule   = UseCCIX
                                                   , elimCH    = False
                                                   , elimCSwap = False
                                                   , druleMap  = emptyDruleMap
                                                   }

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
                              (apply elimWithTof input))
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
                              (length $ apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_cccnot ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test3 = TestCase (assertEqual "elimCtrlsTransformer on CCC(X)."
                              5
                              (length $ apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_cccx ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test4 = TestCase (assertEqual "elimCtrlsTransformer on CC(Y)."
                              5
                              (length $ apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_ccy ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test5 = TestCase (assertEqual "elimCtrlsTransformer on CC(Z)."
                              5
                              (length $ apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_ccz ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test6 = TestCase (assertEqual "elimCtrlsTransformer on CC(H)."
                              5
                              (length $ apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_cch ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test7 = TestCase (assertEqual "elimCtrlsTransformer on CC(swap)."
                              5
                              (length $ apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_ccswp ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

-- Simple Gates (Many Controls)
ascii_cccz = "QGate[\"Z\"](0) with controls=[+1, +2, +3]"

test8 = TestCase (assertEqual "elimCtrlsTransformer on CCC(Z)."
                              9
                              (length $ apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_cccz ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

-- Phase Gates
ascii_gphase    = "GPhase() with t=1"
ascii_cgphase   = "GPhase() with t=2 with controls=[+1]"
ascii_ccgphase  = "GPhase() with t=1.5 with controls=[+1, +2]"
ascii_cccgphase = "GPhase() with t=3 with controls=[+1, +2, +3]"
ascii_ccccgphase = "GPhase() with t=3.5 with controls=[+1, +2, +3, +4]"

abs_gphase   = PhaseGate 1.0 []
abs_cgphase  = PhaseGate 2.0 [Pos 1]
abs_ccgphase = PhaseGate 1.5 [Pos 1, Pos 2]

test9 = TestCase (assertEqual "elimCtrlsTransformer on GPhase()."
                              [abs_gphase]
                              (apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_gphase ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test10 = TestCase (assertEqual "elimCtrlsTransformer on C(GPhase())."
                               [abs_cgphase]
                               (apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_cgphase ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test38 = TestCase (assertEqual "elimCtrlsTransformer on CC(GPhase())."
                               [abs_ccgphase]
                               (apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_ccgphase ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test11 = TestCase (assertEqual "elimCtrlsTransformer on CCC(GPhase())."
                               5
                               (length $ apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_cccgphase ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test12 = TestCase (assertEqual "elimCtrlsTransformer on CCCC(GPhase())."
                               9
                               (length $ apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit\n" ++
                  ascii_ccccgphase ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit"

-- Rotational Gates
ascii_rot    = "QRot[\"exp(-i%Z)\",0.5](0)"
ascii_crot   = "QRot[\"exp(-i%Z)\",1](0) with controls=[+1]"
ascii_ccrot  = "QRot[\"exp(-i%Z)\",2](0) with controls=[+1, +2]"
ascii_cccrot = "QRot[\"exp(-i%Z)\",3](0) with controls=[+1, +2, +3]"

abs_rot  = RotGate RotExpZ False 0.5 [0] []
abs_crot = RotGate RotExpZ False 1.0 [0] [Pos 1]

test13 = TestCase (assertEqual "elimCtrlsTransformer on expZt."
                               [abs_rot]
                               (apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_rot ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test14 = TestCase (assertEqual "elimCtrlsTransformer on C(expZt)."
                               [abs_crot]
                               (apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_crot ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test15 = TestCase (assertEqual "elimCtrlsTransformer on CC(expZt)."
                               5
                               (length $ apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_ccrot ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test16 = TestCase (assertEqual "elimCtrlsTransformer on CCC(expZt)."
                               9
                               (length $ apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_cccrot ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

-- Generalized Controlled QGates
ascii_gctrl = "QGate[\"MyGate\"](0,1); [2,3]"

abs_gctrl = NamedGate (UserDefinedGate "MyGate") False [2, 3, 0, 1] []

test17 = TestCase (assertEqual "elimCtrlsTransformer on QGate with generalized controls."
                               [abs_gctrl]
                               (apply elimWithTof input))
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
                               (apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_omega ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test19 = TestCase (assertEqual "elimCtrlsTransformer on C(omega)."
                               [abs_comega]
                               (apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_comega ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test20 = TestCase (assertEqual "elimCtrlsTransformer on CC(omega)."
                               5
                               (length $ apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_ccomega ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test21 = TestCase (assertEqual "elimCtrlsTransformer on CCC(omega)."
                               9
                               (length $ apply elimWithTof input))
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
                               (apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit\n" ++
                  ascii_w ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit"

test23 = TestCase (assertEqual "elimCtrlsTransformer on C(W)."
                               output
                               (apply elimWithTof input))
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
                               (length $ apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit\n" ++
                  ascii_ccw ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit"

test25 = TestCase (assertEqual "elimCtrlsTransformer on CCC(W)."
                               32
                               (length $ apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit\n" ++
                  ascii_cccw ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit"

-- Special Gates (No Controls)
ascii_e  = "QGate[\"E\"](0)"
ascii_ix = "QGate[\"iX\"](0)"
ascii_s  = "QGate[\"S\"](0)"
ascii_t  = "QGate[\"T\"](0)"
ascii_v  = "QGate[\"V\"](0)"

abs_e  = NamedGate GateE  False [0] []
abs_ix = NamedGate GateIX False [0] []
abs_s  = NamedGate GateS  False [0] []
abs_t  = NamedGate GateT  False [0] []
abs_v  = NamedGate GateV  False [0] []

test26 = TestCase (assertEqual "elimCtrlsTransformer on controll-free special gates."
                               output
                               (apply elimWithTof input))
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
                               (apply elimWithTof input))
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
                               (apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                   ascii_cix ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"
          output = [NamedGate GateX False [0] [Pos 1],
                    NamedGate GateS False [1] []]

test29 = TestCase (assertEqual "elimCtrlsTransformer on C(S)."
                               output
                               (apply elimWithTof input))
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
                               (apply elimWithTof input))
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
                               (apply elimWithTof input))
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
                               (length $ apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit\n" ++
                  ascii_cce ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit"

test33 = TestCase (assertEqual "elimCtrlsTransformer on CCC(E)."
                               18
                               (length $ apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit\n" ++
                  ascii_ccce ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit"

-- Gate Inversions
ascii_h_inv      = "QGate[\"H\"]*(0)"
ascii_s_inv      = "QGate[\"S\"]*(0)"
ascii_w_inv      = "QGate[\"W\"]*(0,1)"
ascii_omega_inv  = "QGate[\"omega\"]*(0)"
ascii_rot_inv    = "QRot[\"exp(-i%Z)\",1]*(0)"
ascii_user_inv   = "QGate[\"MyGate\"]*(0)"
ascii_cs_inv     = "QGate[\"S\"]*(0) with controls=[+1]"
ascii_cw_inv     = "QGate[\"W\"]*(0,1) with controls=[+2]"
ascii_comega_inv = "QGate[\"omega\"]*(0) with controls=[+1]"
ascii_crot_inv   = "QRot[\"MyRot\",1]*(0) with controls=[+1]"

abs_h_inv      = NamedGate GateH                      False         [0]    []
abs_s_inv      = NamedGate GateS                      True          [0]    []
abs_w_inv      = NamedGate GateW                      True          [0, 1] []
abs_omega_inv  = NamedGate GateOmega                  True          [0]    []
abs_comega_inv = PhaseGate                                  (-0.25)        [Pos 1]
abs_rot_inv    = RotGate   RotExpZ                    True  1       [0]    []
abs_user_inv   = NamedGate (UserDefinedGate "MyGate") True          [0]    []

test34 = TestCase (assertEqual "elimCtrlsTransformer supports inverses."
                               output
                               (apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_h_inv     ++ "\n" ++
                  ascii_s_inv     ++ "\n" ++
                  ascii_w_inv     ++ "\n" ++
                  ascii_omega_inv ++ "\n" ++
                  ascii_rot_inv   ++ "\n" ++
                  ascii_user_inv  ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"
          output = [abs_h_inv,
                    abs_s_inv,
                    abs_w_inv,
                    abs_omega_inv,
                    abs_rot_inv,
                    abs_user_inv]

test35 = TestCase (assertEqual "elimCtrlsTransformer on C(S)*."
                               output
                               (apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                   ascii_cs_inv ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"
          output = [NamedGate GateT True  [0] [],
                    NamedGate GateT True  [1] [],
                    NamedGate GateX True  [1] [Pos 0],
                    NamedGate GateT False [1] [],
                    NamedGate GateX True  [1] [Pos 0]]

test36 = TestCase (assertEqual "elimCtrlsTransformer on C(W)*."
                               output
                               (apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                   ascii_cw_inv ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"
          output = [NamedGate GateX True  [1] [Pos 0],
                    NamedGate GateS True  [0] [],
                    NamedGate GateH True  [0] [],
                    NamedGate GateT True  [0] [],
                    NamedGate GateH True  [0] [],
                    NamedGate GateX True  [2] [Pos 1],
                    NamedGate GateX True  [0] [Pos 2],
                    NamedGate GateX True  [1] [Pos 0],
                    NamedGate GateT False [2] [],
                    NamedGate GateT False [1] [],
                    NamedGate GateX True  [1] [Pos 2],
                    NamedGate GateT True  [0] [],
                    NamedGate GateT False [1] [],
                    NamedGate GateX True  [0] [Pos 2],
                    NamedGate GateX True  [1] [Pos 0],
                    NamedGate GateX True  [2] [Pos 1],
                    NamedGate GateT True  [2] [],
                    NamedGate GateT True  [1] [],
                    NamedGate GateT True  [0] [],
                    NamedGate GateH True  [0] [],
                    NamedGate GateT False [0] [],
                    NamedGate GateH True  [0] [],
                    NamedGate GateS False [0] [],
                    NamedGate GateX True  [1] [Pos 0]]

test37 = TestCase (assertEqual "elimCtrlsTransformer on C(omega)*."
                               [abs_comega_inv]
                               (apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                   ascii_comega_inv ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test39 = TestCase (assertEqual "elimCtrlsTransformer on C(expZt)*."
                               output
                               (apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                   ascii_crot_inv ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"
          output = [RotGate   (UserDefinedRot "MyRot") True  0.5 [0] [],
                    NamedGate GateQMultiNot            False     [0] [Pos 1],
                    RotGate   (UserDefinedRot "MyRot") False 0.5 [0] [],
                    NamedGate GateQMultiNot            False     [0] [Pos 1]]

-- User Defined Rotations
ascii_user_rot    = "QRot[\"MyRot\",0.5](0)"
ascii_user_crot   = "QRot[\"MyRot\",1](0) with controls=[+1]"
ascii_user_ccrot  = "QRot[\"MyRot\",2](0) with controls=[+1, +2]"
ascii_user_cccrot = "QRot[\"MyRot\",3](0) with controls=[+1, +2, +3]"

abs_user_rot = RotGate (UserDefinedRot "MyRot") False 0.5 [0] []

test40 = TestCase (assertEqual "elimCtrlsTransformer on MyRot."
                               [abs_user_rot]
                               (apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_user_rot ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test41 = TestCase (assertEqual "elimCtrlsTransformer on C(MyRot)."
                               4
                               (length $ apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_user_crot ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test42 = TestCase (assertEqual "elimCtrlsTransformer on CC(MyRot)."
                               8
                               (length $ apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_user_ccrot ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test43 = TestCase (assertEqual "elimCtrlsTransformer on CCC(MyRot)."
                               12
                               (length $ apply elimWithTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_user_cccrot ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test44 = TestCase (assertEqual "elimCtrlsTransformer on CCX with Toffoli elim (1/2)."
                               toffoliLn
                               (length $ apply elimWithoutTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_ccx ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"
          toffoliLn = 33

containsCCX :: [Gate] -> Bool
containsCCX []                             = False
containsCCX (NamedGate GateX _ _ [_, _]:_)  = True
containsCCX (NamedGate GateIX _ _ [_, _]:_) = True
containsCCX (_:rest)                        = containsCCX rest

test45 = TestCase (assertBool "elimCtrlsTransformer on CCX with Toffoli elim (2/2)."
                               (not $ containsCCX $ apply elimWithoutTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_ccx ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test46 = TestCase (assertEqual "elimCtrlsTransformer on CSwap with Fredkin elim (1/2)."
                               18
                               (length $ apply elimWithoutFredkin input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_cswp ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

containsCSwap :: [Gate] -> Bool
containsCSwap []                             = False
containsCSwap (NamedGate GateSwap _ _ [_]:_) = True
containsCSwap (_:rest)                       = containsCSwap rest

test47 = TestCase (assertBool "elimCtrlsTransformer on CSwap with Fredkin elim (2/2)."
                               (not $ containsCSwap $ apply elimWithoutFredkin input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_cswp ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test48 = TestCase (assertEqual "elimCtrlsTransformer on CH with CH elim (1/2)."
                               7
                               (length $ apply elimWithoutCH input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_ch ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

containsCH :: [Gate] -> Bool
containsCH []                           = False
containsCH (NamedGate GateH _ _ [c1]:_) = True
containsCH (_:rest)                     = containsCH rest

test49 = TestCase (assertBool "elimCtrlsTransformer on CH with CH elim (2/2)."
                               (not $ containsCH $ apply elimWithoutCH input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_ch ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test50 = TestCase (assertEqual "elimCtrlsTransformer with all inlining enabled."
                               (toffoliLn + fredkinLn + hadamarLn)
                               (length $ apply elimMaxInline input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_ccx ++ "\n" ++
                  ascii_ch ++ "\n" ++
                  ascii_cswp ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"
          toffoliLn = 33
          fredkinLn = 18
          hadamarLn = 7

test51 = TestCase (assertEqual "elimCtrlsTransformer on CCCX with Toffoli elim (1/2)."
                               (2 * ccixLn + ancillaCt + toffoliLn)
                               (length $ apply elimWithoutTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_cccx ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"
          toffoliLn = 33
          ccixLn    = 16
          ancillaCt = 2

containsCCCX :: [Gate] -> Bool
containsCCCX []                                 = False
containsCCCX (NamedGate GateX _ _ [_, _, _]:_)  = True
containsCCCX (NamedGate GateIX _ _ [_, _, _]:_) = True
containsCCCX (_:rest)                           = containsCCX rest

containsCCXOrCCCX :: [Gate] -> Bool
containsCCXOrCCCX gates = containsCCX gates || containsCCCX gates

test52 = TestCase (assertBool "elimCtrlsTransformer on CCCX with Toffoli elim (2/2)."
                               (not $ containsCCXOrCCCX $ apply elimWithoutTof input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                  ascii_cccx ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"

test53 = TestCase (assertEqual "elimCtrlsTransformer using CCIX."
                               output
                               (apply elimUseCCIX input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit\n" ++
                   ascii_cccx ++ "\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit"
          output = [QInitGate False 4,
                    NamedGate GateIX False [4] [Pos 1, Pos 2],
                    NamedGate GateX  False [0] [Pos 3, Pos 4],
                    NamedGate GateIX True  [4] [Pos 1, Pos 2],
                    QTermGate False 4]

test54 = TestCase (assertEqual "elimCtrlsTransformer on large multiqnot."
                               output
                               (apply elimUseCCIX input))
    where input = "Inputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit\n" ++
                  "QGate[\"multinot\"](0,1,2) with controls=[+3, +4]\n" ++
                  "Outputs: 0:Qbit, 1:Qbit, 2:Qbit, 3:Qbit, 4:Qbit"
          output = [QInitGate False 5,
                    NamedGate GateIX False        [5]     [Pos 3,Pos 4],
                    NamedGate GateQMultiNot False [0,1,2] [Pos 5],
                    NamedGate GateIX True         [5]     [Pos 3,Pos 4],
                    QTermGate False 5]

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
                                     TestLabel "elimCtrlsTransformer_GPhase_1" test9,
                                     TestLabel "elimCtrlsTransformer_GPhase_2" test10,
                                     TestLabel "elimCtrlsTransformer_GPhase_3" test38,
                                     TestLabel "elimCtrlsTransformer_GPhase_4" test11,
                                     TestLabel "elimCtrlsTransformer_GPhase_5" test12,
                                     TestLabel "elimCtrlsTransformer_QRot_1" test13,
                                     TestLabel "elimCtrlsTransformer_QRot_2" test14,
                                     TestLabel "elimCtrlsTransformer_QRot_3" test15,
                                     TestLabel "elimCtrlsTransformer_QRot_4" test16,
                                     TestLabel "elimCtrlsTransformer_Gen" test17,
                                     TestLabel "elimCtrlsTransformer_Omega_1" test18,
                                     TestLabel "elimCtrlsTransformer_Omega_2" test19,
                                     TestLabel "elimCtrlsTransformer_Omega_3" test20,
                                     TestLabel "elimCtrlsTransformer_Omega_4" test21,
                                     TestLabel "elimCtrlsTransformer_W_1" test22,
                                     TestLabel "elimCtrlsTransformer_W_2" test23,
                                     TestLabel "elimCtrlsTransformer_W_3" test24,
                                     TestLabel "elimCtrlsTransformer_W_4" test25,
                                     TestLabel "elimCtrlsTransformer_QGate_Ex_1" test26,
                                     TestLabel "elimCtrlsTransformer_QGate_Ex_2" test27,
                                     TestLabel "elimCtrlsTransformer_QGate_Ex_3" test28,
                                     TestLabel "elimCtrlsTransformer_QGate_Ex_4" test29,
                                     TestLabel "elimCtrlsTransformer_QGate_Ex_5" test30,
                                     TestLabel "elimCtrlsTransformer_QGate_Ex_6" test31,
                                     TestLabel "elimCtrlsTransformer_QGate_Ex_7" test32,
                                     TestLabel "elimCtrlsTransformer_QGate_Ex_8" test33,
                                     TestLabel "elimCtrlsTransformer_Inv_1" test34,
                                     TestLabel "elimCtrlsTransformer_Inv_2" test35,
                                     TestLabel "elimCtrlsTransformer_Inv_3" test36,
                                     TestLabel "elimCtrlsTransformer_Inv_4" test37,
                                     TestLabel "elimCtrlsTransformer_Inv_5" test39,
                                     TestLabel "elimCtrlsTransformer_User_QRot_1" test40,
                                     TestLabel "elimCtrlsTransformer_User_QRot_2" test41,
                                     TestLabel "elimCtrlsTransformer_User_QRot_3" test42,
                                     TestLabel "elimCtrlsTransformer_User_QRot_4" test43,
                                     TestLabel "elimCtrlsTransformer_elimTof_1" test44,
                                     TestLabel "elimCtrlsTransformer_elimTof_2" test45,
                                     TestLabel "elimCtrlsTransformer_elimCSwap_1" test46,
                                     TestLabel "elimCtrlsTransformer_elimCSwap_2" test47,
                                     TestLabel "elimCtrlsTransformer_elimCH_1" test48,
                                     TestLabel "elimCtrlsTransformer_elimCH_2" test49,
                                     TestLabel "elimCtrlsTransformer_Inlining" test50,
                                     TestLabel "elimCtrlsTransformer_elimTof_3" test51,
                                     TestLabel "elimCtrlsTransformer_elimTof_4" test52,
                                     TestLabel "elimCtrlsTransformer_useCCIX" test53,
                                     TestLabel "elimCtrlsTransformer_multiqnot" test54]

main = defaultMain tests
