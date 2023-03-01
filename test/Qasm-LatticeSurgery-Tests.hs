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
-- * Identity cases.

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
-- * Simple rewrite cases.

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
-- * Rewrites up to global phase.

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
-- * Rejects controls and inversions.

test27 = TestCase (assertEqual "lscRewriteGate rejects inverted gates."
                               (Right UnexpectedInvMod :: Either [Gate] LscGateErr)
                               (lscRewriteGate gate))
    where gate = NamedGate GateSX [] one_op $ negateMod nullGateMod

test28 = TestCase (assertEqual "lscRewriteGate rejects controlled gates."
                               (Right UnexpectedCtrlMod :: Either [Gate] LscGateErr)
                               (lscRewriteGate gate))
    where gate = NamedGate GateSX [] one_op $ addCtrlsToMod 1 nullGateMod

-----------------------------------------------------------------------------------------
-- * Rejects unsupported gate sets.

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
                                     TestLabel "Error_Complex" test31]

main = defaultMain tests
