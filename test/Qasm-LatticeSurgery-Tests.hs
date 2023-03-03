module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.Gate
import LinguaQuanta.Qasm.GateName
import LinguaQuanta.Qasm.Language
import LinguaQuanta.Qasm.LatticeSurgery
import LinguaQuanta.Qasm.Operand

-----------------------------------------------------------------------------------------
-- * Utilities.

one_op = [QRef "q"]
two_op = [QRef "q", QRef "r"]
swp_op = [QRef "r", QRef "q"]

-----------------------------------------------------------------------------------------
-- * lscRewriteGate: Identity Cases.

mk_id_test :: String -> Gate -> Test.HUnit.Test
mk_id_test name gate = TestCase (assertEqual msg exp act)
    where msg = "lscRewriteGate is an identity when applied to: " ++ name
          exp = Left [gate] :: Either [Gate] LscGateErr
          act = lscRewriteGate gate

test1 = mk_id_test "X" $ NamedGate GateX [] one_op nullGateMod
test2 = mk_id_test "Z" $ NamedGate GateZ [] one_op nullGateMod
test3 = mk_id_test "H" $ NamedGate GateH [] one_op nullGateMod
test4 = mk_id_test "T" $ NamedGate GateT [] one_op nullGateMod
test5 = mk_id_test "Tdg" $ NamedGate GateTdg [] one_op nullGateMod
test6 = mk_id_test "S" $ NamedGate GateS [] one_op nullGateMod
test7 = mk_id_test "Sdg" $ NamedGate GateSdg [] one_op nullGateMod

test8 = mk_id_test "RX" $ NamedGate GateRX [Pi] one_op nullGateMod
test9 = mk_id_test "RZ" $ NamedGate GateRZ [Pi] one_op nullGateMod

test10 = mk_id_test "CX" $ NamedGate GateCX [] two_op nullGateMod
test11 = mk_id_test "CZ" $ NamedGate GateCZ [] two_op nullGateMod

test12 = mk_id_test "CRX" $ NamedGate GateCRX [Pi] two_op nullGateMod
test13 = mk_id_test "CRZ" $ NamedGate GateCRZ [Pi] two_op nullGateMod

-----------------------------------------------------------------------------------------
-- * No-op cases.

mk_noop_test :: String -> Gate -> Test.HUnit.Test
mk_noop_test name gate = TestCase (assertEqual msg exp act)
    where msg = "lscRewriteGate maps the following gate to a noop: " ++ name
          exp = Left [] :: Either [Gate] LscGateErr
          act = lscRewriteGate gate

test14 = mk_noop_test "Omega" $ NamedGate GateQuipOmega [] one_op nullGateMod
test15 = mk_noop_test "ID" $ NamedGate GateID [] one_op nullGateMod
test16 = mk_noop_test "GPhase" $ GPhaseGate Pi [] nullGateMod

-----------------------------------------------------------------------------------------
-- * lscRewriteGate: Simple Rewrite Cases.

test17 = TestCase (assertEqual "lscRewriteGate handles swap gates."
                               (Left list :: Either [Gate] LscGateErr)
                               (lscRewriteGate gate))
    where gate = NamedGate GateSwap [] two_op nullGateMod
          list = [NamedGate GateCX [] two_op nullGateMod,
                  NamedGate GateCX [] swp_op nullGateMod,
                  NamedGate GateCX [] two_op nullGateMod]

test18 = TestCase (assertEqual "lscRewriteGate handles CY gates."
                                (Left list :: Either [Gate] LscGateErr)
                                (lscRewriteGate gate))
    where gate = NamedGate GateCY [] swp_op nullGateMod
          list = [NamedGate GateSdg [] one_op nullGateMod,
                  NamedGate GateCX [] swp_op nullGateMod,
                  NamedGate GateS [] one_op nullGateMod]

test19 = TestCase (assertEqual "lscRewriteGate handles SX gates."
                               (Left list :: Either [Gate] LscGateErr)
                               (lscRewriteGate gate))
    where gate = NamedGate GateSX [] one_op nullGateMod
          list = [NamedGate GateH [] one_op nullGateMod,
                  NamedGate GateSdg [] one_op nullGateMod,
                  NamedGate GateH [] one_op nullGateMod]

-----------------------------------------------------------------------------------------
-- * lscRewriteGate: Rewrites Up-to Global Phase.

test20 = TestCase (assertEqual "lscRewriteGate handles Y gates."
                               (Left list :: Either [Gate] LscGateErr)
                               (lscRewriteGate gate))
    where gate = NamedGate GateY [] one_op nullGateMod
          list = [NamedGate GateX [] one_op nullGateMod,
                  NamedGate GateZ [] one_op nullGateMod]

test21 = TestCase (assertEqual "lscRewriteGate handles E gates."
                               (Left list :: Either [Gate] LscGateErr)
                               (lscRewriteGate gate))
    where gate = NamedGate GateQuipE [] one_op nullGateMod
          list = [NamedGate GateH [] one_op nullGateMod,
                  NamedGate GateS [] one_op nullGateMod,
                  NamedGate GateS [] one_op nullGateMod,
                  NamedGate GateS [] one_op nullGateMod]

test22 = TestCase (assertEqual "lscRewriteGate handles iX gates."
                               (Left list :: Either [Gate] LscGateErr)
                               (lscRewriteGate gate))
    where gate = NamedGate GateQuipIX [] one_op nullGateMod
          list = [NamedGate GateX [] one_op nullGateMod]

test23 = TestCase (assertEqual "lscRewriteGate handles P gates."
                               (Left list :: Either [Gate] LscGateErr)
                               (lscRewriteGate gate))
    where gate = NamedGate GateP [Pi] one_op nullGateMod
          list = [NamedGate GateRZ [Pi] one_op nullGateMod]

test24 = TestCase (assertEqual "lscRewriteGate handles CP gates."
                               (Left list :: Either [Gate] LscGateErr)
                               (lscRewriteGate gate))
    where gate = NamedGate GateCP [Pi] two_op nullGateMod
          list = [NamedGate GateCRZ [Pi] two_op nullGateMod]

test25 = TestCase (assertEqual "lscRewriteGate handles Phase gates."
                               (Left list :: Either [Gate] LscGateErr)
                               (lscRewriteGate gate))
    where gate = NamedGate GatePhase [Pi] one_op nullGateMod
          list = [NamedGate GateRZ [Pi] one_op nullGateMod]

test26 = TestCase (assertEqual "lscRewriteGate handles CPhase gates."
                               (Left list :: Either [Gate] LscGateErr)
                               (lscRewriteGate gate))
    where gate = NamedGate GateCPhase [Pi] two_op nullGateMod
          list = [NamedGate GateCRZ [Pi] two_op nullGateMod]

-----------------------------------------------------------------------------------------
-- * lscRewriteGate: Rejects Controls and Inversions.

test27 = TestCase (assertEqual "lscRewriteGate rejects inverted gates."
                               (Right UnexpectedInvMod :: Either [Gate] LscGateErr)
                               (lscRewriteGate gate))
    where gate = NamedGate GateSX [] one_op $ negateMod nullGateMod

test28 = TestCase (assertEqual "lscRewriteGate rejects controlled gates."
                               (Right UnexpectedCtrlMod :: Either [Gate] LscGateErr)
                               (lscRewriteGate gate))
    where gate = NamedGate GateSX [] one_op $ addCtrlsToMod 1 nullGateMod

-----------------------------------------------------------------------------------------
-- * lscRewriteGate: Rejects Unsupported Gate Sets.

test29 = TestCase (assertEqual "lscRewriteGate rejects named gates."
                               (Right errs :: Either [Gate] LscGateErr)
                               (lscRewriteGate gate))
    where gate = NamedGate (UserDefined "mygate") [] one_op nullGateMod
          errs = UnknownCompilation "mygate"

test30 = TestCase (assertEqual "lscRewriteGate rejects U-gates."
                               (Right errs :: Either [Gate] LscGateErr)
                               (lscRewriteGate gate))
    where gate = NamedGate GateU [] one_op nullGateMod
          errs = UnsupportedCompilation GateU

test31 = TestCase (assertEqual "lscRewriteGate rejects gates with complex decomps."
                               (Right errs :: Either [Gate] LscGateErr)
                               (lscRewriteGate gate))
    where gate = NamedGate GateQuipW [] one_op nullGateMod
          errs = UnsupportedCompilation GateQuipW

-----------------------------------------------------------------------------------------
-- * MergedVarMap.

map1 = makeMergedVarMap "qvar" "cvar"
map2 = addQDecl map1 "q1" 1
map3 = addCDecl map2 "c1" 5
map4 = addQDecl map3 "q2" 6
map5 = addCDecl map4 "c2" 3
map6 = addQDecl map5 "q3" 4
map7 = addCDecl map6 "c3" 7

test32 = TestCase (assertEqual "toQDecl returns the correct declarations (1/7)"
                               ("qvar", 0)
                               (toQDecl map1))

test33 = TestCase (assertEqual "toQDecl returns the correct declarations (2/7)"
                               ("qvar", 1)
                               (toQDecl map2))

test34 = TestCase (assertEqual "toQDecl returns the correct declarations (3/7)"
                               ("qvar", 1)
                               (toQDecl map3))

test35 = TestCase (assertEqual "toQDecl returns the correct declarations (4/7)"
                               ("qvar", 7)
                               (toQDecl map4))

test36 = TestCase (assertEqual "toQDecl returns the correct declarations (5/7)"
                               ("qvar", 7)
                               (toQDecl map5))

test37 = TestCase (assertEqual "toQDecl returns the correct declarations (6/7)"
                               ("qvar", 11)
                               (toQDecl map6))

test38 = TestCase (assertEqual "toQDecl returns the correct declarations (7/7)"
                               ("qvar", 11)
                               (toQDecl map7))

test39 = TestCase (assertEqual "toCDecl returns the correct declarations (1/7)"
                               ("cvar", 0)
                               (toCDecl map1))

test40 = TestCase (assertEqual "toCDecl returns the correct declarations (2/7)"
                               ("cvar", 0)
                               (toCDecl map2))

test41 = TestCase (assertEqual "toCDecl returns the correct declarations (3/7)"
                               ("cvar", 5)
                               (toCDecl map3))

test42 = TestCase (assertEqual "toCDecl returns the correct declarations (4/7)"
                               ("cvar", 5)
                               (toCDecl map4))

test43 = TestCase (assertEqual "toCDecl returns the correct declarations (5/7)"
                               ("cvar", 8)
                               (toCDecl map5))

test44 = TestCase (assertEqual "toCDecl returns the correct declarations (6/7)"
                               ("cvar", 8)
                               (toCDecl map6))

test45 = TestCase (assertEqual "toCDecl returns the correct declarations (7/7)"
                               ("cvar", 15)
                               (toCDecl map7))

test46 = TestCase (assertEqual "lookupQVar with in-scope declaration (1/2)."
                               (Just ("qvar", 3) :: Maybe (String, Int))
                               (lookupQVar map7 "q2" 2))

test47 = TestCase (assertEqual "lookupQVar with in-scope declaration (2/2)."
                               (Just ("qvar", 4) :: Maybe (String, Int))
                               (lookupQVar map7 "q2" 3))

test48 = TestCase (assertEqual "lookupQVar with out-of-scope declaration."
                               (Nothing :: Maybe (String, Int))
                               (lookupQVar map1 "q2" 3))

test49 = TestCase (assertEqual "lookupCVar with in-scope declaration (1/2)."
                               (Just ("cvar", 8) :: Maybe (String, Int))
                               (lookupCVar map7 "c2" 3))

test50 = TestCase (assertEqual "lookupCVar with in-scope declaration (2/2)."
                               (Just ("cvar", 9) :: Maybe (String, Int))
                               (lookupCVar map7 "c2" 4))

test51 = TestCase (assertEqual "lookupCVar with out-of-scope declaration."
                               (Nothing :: Maybe (String, Int))
                               (lookupCVar map1 "c2" 3))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "GateX" test1,
                                     TestLabel "GateZ" test2,
                                     TestLabel "GateH" test3,
                                     TestLabel "GateT" test4,
                                     TestLabel "GateTdg" test5,
                                     TestLabel "GateS" test6,
                                     TestLabel "GateSdg" test7,
                                     TestLabel "GateRX" test8,
                                     TestLabel "GateRZ" test9,
                                     TestLabel "GateCX" test10,
                                     TestLabel "GateCZ" test11,
                                     TestLabel "GateCRX" test12,
                                     TestLabel "GateCRZ" test13,
                                     TestLabel "GateQuipOmega" test14,
                                     TestLabel "GateID" test15,
                                     TestLabel "GPhase" test16,
                                     TestLabel "GateSwap" test17,
                                     TestLabel "GateCY" test18,
                                     TestLabel "GateSX" test19,
                                     TestLabel "GateY" test20,
                                     TestLabel "GateE" test21,
                                     TestLabel "GateIX" test22,
                                     TestLabel "GateP" test23,
                                     TestLabel "GateCP" test24,
                                     TestLabel "GatePhase" test25,
                                     TestLabel "GateCPhase" test26,
                                     TestLabel "Error_Inv" test27,
                                     TestLabel "Error_Ctrl" test28,
                                     TestLabel "Error_UserDefined" test29,
                                     TestLabel "Error_UGate" test30,
                                     TestLabel "Error_Complex" test31,
                                     TestLabel "toQDecl_1" test32,
                                     TestLabel "toQDecl_2" test33,
                                     TestLabel "toQDecl_3" test34,
                                     TestLabel "toQDecl_4" test35,
                                     TestLabel "toQDecl_5" test36,
                                     TestLabel "toQDecl_6" test37,
                                     TestLabel "toQDecl_7" test38,
                                     TestLabel "toCDecl_1" test39,
                                     TestLabel "toCDecl_2" test40,
                                     TestLabel "toCDecl_3" test41,
                                     TestLabel "toCDecl_4" test42,
                                     TestLabel "toCDecl_5" test43,
                                     TestLabel "toCDecl_6" test44,
                                     TestLabel "toCDecl_7" test45,
                                     TestLabel "lookupQVar_Match_1" test46,
                                     TestLabel "lookupQVar_Match_2" test47,
                                     TestLabel "lookupQVar_NoMatch" test48,
                                     TestLabel "lookupCVar_Match_1" test49,
                                     TestLabel "lookupCVar_Match_2" test50,
                                     TestLabel "lookupCVar_NoMatch" test51]

main = defaultMain tests
