module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Qasm.Expression
import Qasm.Gate
import Qasm.GateName
import Qasm.Inversion
import Qasm.Language

-----------------------------------------------------------------------------------------
-- Useful Constructions

mod1 = GateMod False []
mod2 = GateMod False [Pos, Neg]
mod3 = GateMod False [Neg, Pos, Pos]

name1 = UserDefined "xyz"
name2 = UserDefined "qrs"
name3 = UserDefined "mygate"

angle1 = Pi
angle2 = Div Pi (DecInt "2")
angle3 = Plus (Div (DecInt "2") Pi) Pi
angle4 = Times Pi Pi

neg_angle1 = negateExpr angle1
neg_angle2 = negateExpr angle2
neg_angle3 = negateExpr angle3
neg_angle4 = negateExpr angle4

params_len1 = [angle1]
params_len2 = [angle1, angle2]
params_len3 = [angle1, angle2, angle3]
params_len4 = [angle1, angle2, angle3, angle4]

ops_len1 = [QVar "x0"]
ops_len2 = [QVar "x0", QVar "x1"]
ops_len3 = [QVar "x0", QVar "x1", QReg "reg" (DecInt "7")]
ops_len4 = [QVar "x0", QVar "x1", QReg "reg" (DecInt "7"), QVar "x2"]

build_g1 :: [Expr] -> Gate
build_g1 params = NamedGate name1 params [] mod1

build_g2 :: [Expr] -> Gate
build_g2 params = NamedGate name2 params ops_len1 mod2

build_g3 :: [Expr] -> Gate
build_g3 params = NamedGate name3 params ops_len2 mod3

build_g4 :: [Expr] -> Gate
build_g4 params = NamedGate name3 params ops_len3 mod3

build_pg1 :: Expr -> Gate
build_pg1 angle = GPhaseGate angle [] mod1

build_pg2 :: Expr -> Gate
build_pg2 angle = GPhaseGate angle ops_len3 mod3

-----------------------------------------------------------------------------------------
-- negateParams

nparams_len1 = [neg_angle1]
nparams_len2 = [neg_angle1, neg_angle2]
nparams_len3 = [neg_angle1, neg_angle2, neg_angle3]
nparams_len4 = [neg_angle1, neg_angle2, neg_angle3, neg_angle4]

test1 = TestCase (assertEqual "negataParams supports 0 param named gates."
                              (build_g1 [])
                              (negateParams $ build_g1 []))

test2 = TestCase (assertEqual "negataParams supports 1 param named gates."
                              (build_g2 nparams_len1)
                              (negateParams $ build_g2 params_len1))

test3 = TestCase (assertEqual "negataParams supports 2 param named gates."
                              (build_g3 nparams_len2)
                              (negateParams $ build_g3 params_len2))

test4 = TestCase (assertEqual "negataParams supports 3 param named gates."
                              (build_g4 nparams_len3)
                              (negateParams $ build_g4 params_len3))

test5 = TestCase (assertEqual "negataParams supports phase gates (1/2)."
                              (build_pg1 neg_angle1)
                              (negateParams $ build_pg1 angle1))

test6 = TestCase (assertEqual "negataParams supports phase gates (2/2)."
                              (build_pg2 neg_angle2)
                              (negateParams $ build_pg2 angle2))

-----------------------------------------------------------------------------------------
-- threeParamInversion

set2_params1_pre = [angle1, angle2, angle3]
set2_params1_res = [negateExpr angle1, negateExpr angle3, negateExpr angle2]
set2_params2_pre = [angle4, angle3, angle1]
set2_params2_res = [negateExpr angle4, negateExpr angle1, negateExpr angle3]

test7 = TestCase (assertEqual "threeParamInversion requires 3 parameters (1/5)."
                              Nothing
                              (threeParamInversion $ build_g1 []))

test8 = TestCase (assertEqual "threeParamInversion requires 3 parameters (2/5)."
                              Nothing
                              (threeParamInversion $ build_g2 params_len1))

test9 = TestCase (assertEqual "threeParamInversion requires 3 parameters (3/5)."
                              Nothing
                              (threeParamInversion $ build_g3 params_len2))

test10 = TestCase (assertEqual "threeParamInversion requires 3 parameters (4/5)."
                               Nothing
                               (threeParamInversion $ build_g4 params_len4))

test11 = TestCase (assertEqual "threeParamInversion requires 3 parameters (5/5)."
                               Nothing
                               (threeParamInversion $ build_pg1 angle1))

test12 = TestCase (assertEqual "threeParamInversion performs correct inversions (1/2)."
                               (Just (build_g1 set2_params1_res))
                               (threeParamInversion $ build_g1 set2_params1_pre))

test13 = TestCase (assertEqual "threeParamInversion performs correct inversions (2/2)."
                               (Just (build_g2 set2_params2_res))
                               (threeParamInversion $ build_g2 set2_params2_pre))

-----------------------------------------------------------------------------------------
-- oneParamInversion

set3_g1_pre = NamedGate GatePhase [angle1] ops_len1 mod1
set3_g1_res = NamedGate GateU [zero, negateExpr angle1, zero] ops_len1 mod1
set3_g2_pre = NamedGate GateCPhase [angle1] ops_len4 mod2
set3_g2_res = NamedGate GateCU [zero, negateExpr angle1, zero] ops_len4 mod2

test14 = TestCase (assertEqual "oneParamInversion requires 1 parameter (1/5)."
                              Nothing
                              (oneParamInversion (build_g1 []) True))

test15 = TestCase (assertEqual "oneParamInversion requires 1 parameter (2/5)."
                              Nothing
                              (oneParamInversion (build_g1 params_len2) False))

test16 = TestCase (assertEqual "oneParamInversion requires 1 parameter (3/5)."
                              Nothing
                              (oneParamInversion (build_g1 params_len3) True))

test17 = TestCase (assertEqual "oneParamInversion requires 1 parameter (4/5)."
                              Nothing
                              (oneParamInversion (build_g1 params_len4) False))

test18 = TestCase (assertEqual "oneParamInversion requires 1 parameter (5/5)."
                              Nothing
                              (oneParamInversion (build_pg1 angle1) True))

test19 = TestCase (assertEqual "oneParamInversion requires a single operand."
                              Nothing
                              (oneParamInversion (build_g3 params_len1) False))

test20 = TestCase (assertEqual "oneParamInversion requires controls."
                              Nothing
                              (oneParamInversion (build_g4 params_len1) True))

test21 = TestCase (assertEqual "oneParamInversion converts to U(0, a, 0)."
                              (Just set3_g1_res)
                              (oneParamInversion set3_g1_pre False))

test22 = TestCase (assertEqual "oneParamInversion converts to CU(0, a, 0)."
                              (Just set3_g2_res)
                              (oneParamInversion set3_g2_pre True))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "negateParams_Named_0Param" test1,
                                     TestLabel "negateParams_Named_1Param" test2,
                                     TestLabel "negateParams_Named_2Param" test3,
                                     TestLabel "negateParams_Named_3Param" test4,
                                     TestLabel "negateParams_GPhase_Test1" test5,
                                     TestLabel "negateParams_GPhase_Test2" test6,
                                     TestLabel "threeParamInversion_Reg3_Test1" test7,
                                     TestLabel "threeParamInversion_Reg3_Test2" test8,
                                     TestLabel "threeParamInversion_Reg3_Test3" test9,
                                     TestLabel "threeParamInversion_Reg3_Test4" test10,
                                     TestLabel "threeParamInversion_Reg3_Test5" test11,
                                     TestLabel "threeParamInversion_Inv_Test1" test12,
                                     TestLabel "threeParamInversion_Inv_Test2" test13,
                                     TestLabel "threeParamInversion_Reg1_Test1" test14,
                                     TestLabel "threeParamInversion_Reg1_Test2" test15,
                                     TestLabel "threeParamInversion_Reg1_Test3" test16,
                                     TestLabel "threeParamInversion_Reg1_Test4" test17,
                                     TestLabel "threeParamInversion_Reg1_Test5" test18,
                                     TestLabel "threeParamInversion_BadOperands" test19,
                                     TestLabel "threeParamInversion_BadControls" test20,
                                     TestLabel "threeParamInversion_NoControls" test21,
                                     TestLabel "threeParamInversion_WithControls" test22]

main = defaultMain tests
