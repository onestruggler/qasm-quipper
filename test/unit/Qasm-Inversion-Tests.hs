module Main where

import Data.Maybe
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.Expression
import LinguaQuanta.Qasm.Gate
import LinguaQuanta.Qasm.GateName
import LinguaQuanta.Qasm.Inversion
import LinguaQuanta.Qasm.Language

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

ops_len1 = [QRef "x0"]
ops_len2 = [QRef "x0", QRef "x1"]
ops_len3 = [QRef "x0", QRef "x1", Cell "reg" 7]
ops_len4 = [QRef "x0", QRef "x1", Cell "reg" 7, QRef "x2"]

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
set2_params1_res = [neg_angle1, neg_angle3, neg_angle2]
set2_params2_pre = [angle4, angle3, angle1]
set2_params2_res = [neg_angle4, neg_angle1, neg_angle3]

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
-- invertGate

set4_mk_operands :: [Sign] -> GateName -> [Operand]
set4_mk_operands ctrl name = impl (ctrlCt + argsCt)
    where ctrlCt = length ctrl
          argsCt = fromJust $ toOperandCount name
          impl n = if n == 0 then [] else QRef ("var" ++ show n) : impl (n - 1)

set4_mktest_selfinv :: GateName -> Test.HUnit.Test
set4_mktest_selfinv name = TestCase (assertEqual msg (Just [invert gate]) res)
    where msg  = "invertGate supports self-inverse gates (" ++ show name ++ ")."
          ctrl = [Pos, Neg]
          gate = NamedGate name [] (set4_mk_operands ctrl name) (GateMod True ctrl)
          res  = invertGate gate

test23 = set4_mktest_selfinv GateX
test24 = set4_mktest_selfinv GateCX
test25 = set4_mktest_selfinv GateCCX
test26 = set4_mktest_selfinv GateY
test27 = set4_mktest_selfinv GateCY
test28 = set4_mktest_selfinv GateZ
test29 = set4_mktest_selfinv GateCZ
test30 = set4_mktest_selfinv GateH
test31 = set4_mktest_selfinv GateCH
test32 = set4_mktest_selfinv GateSwap
test33 = set4_mktest_selfinv GateCSwap
test34 = set4_mktest_selfinv GateID
test35 = set4_mktest_selfinv GateQuipW

set4_mk_params :: GateName -> [Expr]
set4_mk_params name = impl (fromJust $ toParamCount name)
    where impl n = if n == 0 then [] else (Div Pi (DecInt $ show (n + 1))) : impl (n - 1)

set4_mktest_paraminv :: GateName -> Test.HUnit.Test
set4_mktest_paraminv name = TestCase (assertEqual msg (Just [inv]) res)
    where msg   = "invertGate supports inverse by param negation (" ++ show name ++ ")."
          ctrl  = [Pos, Neg]
          param = set4_mk_params name
          ops   = set4_mk_operands ctrl name
          mod   = GateMod True ctrl
          inv   = NamedGate name (map negateExpr param) ops (GateMod False ctrl)
          res   = invertGate (NamedGate name param ops (GateMod True ctrl))

test36 = set4_mktest_paraminv GateRX
test37 = set4_mktest_paraminv GateCRX
test38 = set4_mktest_paraminv GateRY
test39 = set4_mktest_paraminv GateCRY
test40 = set4_mktest_paraminv GateRZ
test41 = set4_mktest_paraminv GateCRZ
test42 = set4_mktest_paraminv GateP
test43 = set4_mktest_paraminv GateCP
test14 = set4_mktest_paraminv GateU1
test15 = set4_mktest_paraminv GatePhase
test16 = set4_mktest_paraminv GateCPhase

set4_mktest_invpair :: GateName -> GateName -> Test.HUnit.Test
set4_mktest_invpair name iname = TestCase (assertEqual msg (Just [inv]) res)
    where msg  = "invertGate identifies pairs of inverse gates (" ++ show name ++ ")."
          ctrl = [Pos, Neg]
          ops  = set4_mk_operands ctrl name
          inv  = NamedGate iname [] ops (GateMod False ctrl)
          res  = invertGate (NamedGate name [] ops (GateMod True ctrl))

test45 = set4_mktest_invpair GateS GateSdg
test46 = set4_mktest_invpair GateSdg GateS
test47 = set4_mktest_invpair GateT GateTdg
test48 = set4_mktest_invpair GateTdg GateT

test52 = TestCase (assertEqual "invertGate applied to U."
                               (Just [NamedGate GateU iparams ops negMod])
                               (invertGate (NamedGate GateU params ops mod)))
    where ctrl    = [Pos, Neg]
          ops     = set4_mk_operands ctrl GateU
          mod     = GateMod True ctrl
          negMod  = negateMod mod
          params  = [angle1, angle2, angle3]
          iparams = [neg_angle1, neg_angle3, neg_angle2]

test53 = TestCase (assertEqual "invertGate applied to CU."
                               (Just [NamedGate GateCU iparams ops negMod])
                               (invertGate (NamedGate GateCU params ops mod)))
    where ctrl    = [Pos, Neg]
          ops     = set4_mk_operands ctrl GateCU
          mod     = GateMod True ctrl
          negMod  = negateMod mod
          params  = [angle1, angle2, angle3, angle4]
          iparams = [neg_angle1, neg_angle3, neg_angle2, neg_angle4]

test54 = TestCase (assertEqual "invertGate applied to U3."
                               (Just [NamedGate GateU3 iparams ops negMod])
                               (invertGate (NamedGate GateU3 params ops mod)))
    where ctrl    = [Pos, Neg]
          ops     = set4_mk_operands ctrl GateU3
          mod     = GateMod True ctrl
          negMod  = negateMod mod
          params  = [angle1, angle2, angle3]
          iparams = [neg_angle1, neg_angle3, neg_angle2]

test55 = TestCase (assertEqual "invertGate applied to U2."
                               (Just [NamedGate GateU3 iparams ops negMod])
                               (invertGate (NamedGate name params ops mod)))
    where name    = GateU2
          ctrl    = [Pos, Neg]
          ops     = set4_mk_operands ctrl name
          mod     = GateMod True ctrl
          negMod  = negateMod mod
          halfPi  = Div Pi (DecInt "2")
          params  = [angle1, angle2]
          iparams = [negateExpr halfPi, neg_angle2, neg_angle1]

test56 = TestCase (assertEqual "invertGate applied to Omega."
                               (Just [GPhaseGate sevenFourthsPi ops negMod])
                               (invertGate (NamedGate name [] ops mod)))
    where name           = GateQuipOmega
          ctrl           = [Pos, Neg]
          ops            = set4_mk_operands ctrl name
          mod            = GateMod True ctrl
          negMod         = negateMod mod
          sevenFourthsPi = Times (Div (DecInt "7") (DecInt "4")) Pi

test57 = TestCase (assertEqual "invertGate applied to SX."
                               (Just [NamedGate GateSX [] ops negMod,
                                      NamedGate GateX [] ops negMod])
                               (invertGate (NamedGate name [] ops mod)))
    where name   = GateSX
          ctrl   = [Pos, Neg]
          ops    = set4_mk_operands ctrl name
          mod    = GateMod True ctrl
          negMod = negateMod mod

test58 = TestCase (assertEqual "invertGate applied to E."
                               (Just [GPhaseGate fiveFourthsPi ops negMod,
                                      NamedGate GateH [] ops negMod,
                                      NamedGate GateS [] ops negMod])
                               (invertGate (NamedGate name [] ops mod)))
    where name          = GateQuipE
          ctrl          = [Pos, Neg]
          ops           = set4_mk_operands ctrl name
          mod           = GateMod True ctrl
          negMod        = negateMod mod
          fiveFourthsPi = Times (Div (DecInt "5") (DecInt "4")) Pi

test59 = TestCase (assertEqual "invertGate applied to E."
                               (Just [GPhaseGate Pi ops negMod,
                                      NamedGate GateQuipIX [] ops negMod])
                               (invertGate (NamedGate name [] ops mod)))
    where name          = GateQuipIX
          ctrl          = [Pos, Neg]
          ops           = set4_mk_operands ctrl name
          mod           = GateMod True ctrl
          negMod        = negateMod mod

test60 = TestCase (assertEqual "invertGate rejects inverted user-defined gates."
                               Nothing
                               (invertGate (NamedGate name [] ops mod)))
    where name = UserDefined "gate"
          ctrl = [Pos, Neg]
          ops  = set4_mk_operands ctrl name
          mod  = GateMod True ctrl

test61 = TestCase (assertEqual "invertGate accepts non-inverted user-defined gates."
                               (Just [gate])
                               (invertGate gate))
    where name = UserDefined "gate"
          ctrl = [Pos, Neg]
          gate = NamedGate name [] ops_len4 (GateMod False ctrl)

test62 = TestCase (assertEqual "invertGate rejects gates without known inverses."
                               Nothing
                               (invertGate (NamedGate name params ops mod)))
    where name   = GateU
          ctrl   = [Pos, Neg]
          params = [zero, zero, zero, zero]
          ops    = set4_mk_operands ctrl name
          mod    = GateMod True ctrl

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
                                     TestLabel "invertGate_SelfInverse_Test1" test23,
                                     TestLabel "invertGate_SelfInverse_Test2" test24,
                                     TestLabel "invertGate_SelfInverse_Test3" test25,
                                     TestLabel "invertGate_SelfInverse_Test4" test26,
                                     TestLabel "invertGate_SelfInverse_Test5" test27,
                                     TestLabel "invertGate_SelfInverse_Test6" test28,
                                     TestLabel "invertGate_SelfInverse_Test7" test29,
                                     TestLabel "invertGate_SelfInverse_Test8" test30,
                                     TestLabel "invertGate_SelfInverse_Test9" test31,
                                     TestLabel "invertGate_SelfInverse_Test10" test32,
                                     TestLabel "invertGate_SelfInverse_Test11" test33,
                                     TestLabel "invertGate_SelfInverse_Test12" test34,
                                     TestLabel "invertGate_SelfInverse_Test13" test35,
                                     TestLabel "invertGate_ParamInverse_Test1" test36,
                                     TestLabel "invertGate_ParamInverse_Test2" test37,
                                     TestLabel "invertGate_ParamInverse_Test3" test38,
                                     TestLabel "invertGate_ParamInverse_Test4" test39,
                                     TestLabel "invertGate_ParamInverse_Test5" test40,
                                     TestLabel "invertGate_ParamInverse_Test6" test41,
                                     TestLabel "invertGate_ParamInverse_Test7" test42,
                                     TestLabel "invertGate_ParamInverse_Test8" test43,
                                     TestLabel "invertGate_ParamInverse_Test9" test14,
                                     TestLabel "invertGate_ParamInverse_Test10" test15,
                                     TestLabel "invertGate_ParamInverse_Test11" test16,
                                     TestLabel "invertGate_InversePairs_Test1" test45,
                                     TestLabel "invertGate_InversePairs_Test2" test46,
                                     TestLabel "invertGate_InversePairs_Test3" test47,
                                     TestLabel "invertGate_InversePairs_Test4" test48,
                                     TestLabel "invertGate_U" test52,
                                     TestLabel "invertGate_CU" test53,
                                     TestLabel "invertGate_U3" test54,
                                     TestLabel "invertGate_U2" test55,
                                     TestLabel "invertGate_Omega" test56,
                                     TestLabel "invertGate_SX" test57,
                                     TestLabel "invertGate_E" test58,
                                     TestLabel "invertGate_iX" test59,
                                     TestLabel "invertGate_UserDefined_W_Inv" test60,
                                     TestLabel "invertGate_UserDefined_WO_Inv" test61,
                                     TestLabel "invertGate_Unknown" test62]

main = defaultMain tests
