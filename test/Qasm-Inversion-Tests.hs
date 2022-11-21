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

ops_len0 :: [GateOperand]
ops_len0 = []
ops_len1 = [QVar "x0"]
ops_len2 = [QVar "x0", QVar "x1"]
ops_len3 = [QVar "x0", QVar "x1", QReg "reg" (DecInt "7")]

-----------------------------------------------------------------------------------------
-- negateParams

params_len0 :: [Expr]
params_len0 = []
params_len1 = [angle1]
params_len2 = [angle1, angle2]
params_len3 = [angle1, angle2, angle3]
params_len4 = [angle1, angle2, angle3, angle4]

nparams_len0 :: [Expr]
nparams_len0 = []
nparams_len1 = [neg_angle1]
nparams_len2 = [neg_angle1, neg_angle2]
nparams_len3 = [neg_angle1, neg_angle2, neg_angle3]
nparams_len4 = [neg_angle1, neg_angle2, neg_angle3, neg_angle4]

set1_build_g1 :: [Expr] -> Gate
set1_build_g1 params = NamedGate name1 params ops_len0 mod1

set1_build_g2 :: [Expr] -> Gate
set1_build_g2 params = NamedGate name2 params ops_len1 mod2

set1_build_g3 :: [Expr] -> Gate
set1_build_g3 params = NamedGate name3 params ops_len2 mod3

set1_build_pg :: Expr -> Gate
set1_build_pg angle = GPhaseGate angle ops_len0 mod1

test1 = TestCase (assertEqual "negataParams supports 0 param named gates."
                              (set1_build_g1 nparams_len0)
                              (negateParams (set1_build_g1 params_len0)))

test2 = TestCase (assertEqual "negataParams supports 1 param named gates."
                              (set1_build_g2 nparams_len1)
                              (negateParams (set1_build_g2 params_len1)))

test3 = TestCase (assertEqual "negataParams supports 2 param named gates."
                              (set1_build_g3 nparams_len2)
                              (negateParams (set1_build_g3 params_len2)))

test4 = TestCase (assertEqual "negataParams supports 3 param named gates."
                              (set1_build_g1 nparams_len3)
                              (negateParams (set1_build_g1 params_len3)))

test5 = TestCase (assertEqual "negataParams supports phase gates (1/2)."
                              (set1_build_pg neg_angle1)
                              (negateParams (set1_build_pg angle1)))

test6 = TestCase (assertEqual "negataParams supports phase gates (2/2)."
                              (set1_build_pg neg_angle2)
                              (negateParams (set1_build_pg angle2)))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "negateParams_Named_0Param" test1,
                                     TestLabel "negateParams_Named_1Param" test2,
                                     TestLabel "negateParams_Named_2Param" test3,
                                     TestLabel "negateParams_Named_3Param" test4,
                                     TestLabel "negateParams_GPhase_Test1" test5,
                                     TestLabel "negateParams_GPhase_Test2" test6]

main = defaultMain tests
