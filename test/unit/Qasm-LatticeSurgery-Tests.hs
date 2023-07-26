module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Either
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
-- * toMergedSize.

test52 = TestCase (assertEqual "toMergedSize works on Just constructors."
                               1
                               (toMergedSize Nothing))

test53 = TestCase (assertEqual "toMergedSize works on Nothing constructors."
                               10
                               (toMergedSize $ Just 10))

-----------------------------------------------------------------------------------------
-- * lookupOperand.

test54 = TestCase (assertEqual "lookupOperand handles in-scope QRef operands."
                               (Left $ Cell "qvar" 0 :: Either Operand String)
                               (lookupOperand map7 $ QRef "q1"))

test55 = TestCase (assertEqual "lookupOperand handles in-scope Cell operands."
                               (Left $ Cell "qvar" 4 :: Either Operand String)
                               (lookupOperand map7 $ Cell "q2" 3))

test56 = TestCase (assertEqual "lookupOperand handles out-of-scope QRef operands."
                               (Right "qqq" :: Either Operand String)
                               (lookupOperand map7 $ QRef "qqq"))

test57 = TestCase (assertEqual "lookupOperand handles out-of-scope Cell operands."
                               (Right "vvv" :: Either Operand String)
                               (lookupOperand map7 $ Cell "vvv" 3))

-----------------------------------------------------------------------------------------
-- * lookupOperand.

test58 = TestCase (assertEqual "lookupOperand handles in-scope mixed lists."
                               (Left result :: Either [Operand] String)
                               (leftMap (lookupOperand map7) inputs))
    where inputs = [QRef "q1", Cell "q2" 1, Cell "q2" 3]
          result = [Cell "qvar" 0, Cell "qvar" 2, Cell "qvar" 4]

test59 = TestCase (assertEqual "lookupOperand handles lists with out-of-scope operands."
                               (Right "vvv" :: Either [Operand] String)
                               (leftMap (lookupOperand map7) inputs))
    where inputs = [QRef "q1", Cell "vvv" 1, Cell "q2" 3]

-----------------------------------------------------------------------------------------
-- * lookupCtorOperand.

ctor :: Operand -> String
ctor (QRef id)     = id
ctor (Cell id idx) = id ++ "[" ++ show idx ++ "]"

test60 = TestCase (assertEqual "lookupCtorOperand supports valid operands."
                               (Left "qvar[0]" :: Either String String)
                               (lookupCtorOperand map7 ctor $ QRef "q1"))

test61 = TestCase (assertEqual "lookupCtorOperand rejects invalid operands."
                               (Right "vvv" :: Either String String)
                               (lookupCtorOperand map7 ctor $ QRef "vvv"))

-----------------------------------------------------------------------------------------
-- * updateExpr.

mk_expr t1 t2 = Plus (Brack (Div (Times (Minus Euler Pi) (DecFloat "0.5")) t1))
                     (Negate (Times Tau (Times (DecInt "1") t2)))

test62 = TestCase (assertEqual "updateExpr supports QasmId expressions."
                               (Left $ mk_expr oexpr1 oexpr2 :: Either Expr String)
                               (updateExpr map7 $ mk_expr iexpr1 iexpr2))
    where iexpr1 = QasmId "q1"
          iexpr2 = QasmId "c2"
          oexpr1 = QasmCell "qvar" $ Plus (DecInt "0") (DecInt "0")
          oexpr2 = QasmCell "cvar" $ Plus (DecInt "0") (DecInt "5")

test63 = TestCase (assertEqual "updateExpr supports QasmCell expressions."
                               (Left $ mk_expr oexpr1 oexpr2 :: Either Expr String)
                               (updateExpr map7 $ mk_expr iexpr1 iexpr2))
    where index1 = DecInt "2"
          index2 = Plus (DecInt "1") (DecInt "2")
          iexpr1 = QasmCell "q2" index1
          iexpr2 = QasmCell "c2" index2
          oexpr1 = QasmCell "qvar" $ Plus index1 (DecInt "1")
          oexpr2 = QasmCell "cvar" $ Plus index2 (DecInt "5")

test64 = TestCase (assertEqual "updateExpr supports QasmMeasure expressions."
                               (Left $ mk_expr oexpr1 oexpr2 :: Either Expr String)
                               (updateExpr map7 $ mk_expr iexpr1 iexpr2))
    where index2 = DecInt "2"
          iexpr1 = QasmMeasure $ QVar "q1"
          iexpr2 = QasmMeasure $ QReg "q2" index2
          oexpr1 = QasmMeasure $ QReg "qvar" $ Plus (DecInt "0") (DecInt "0")
          oexpr2 = QasmMeasure $ QReg "qvar" $ Plus index2 (DecInt "1")

test65 = TestCase (assertEqual "updateExpr supports Call expressions."
                               (Left $ mk_expr oexpr1 oexpr2 :: Either Expr String)
                               (updateExpr map7 $ mk_expr iexpr1 iexpr2))
    where iexpr1 = Call "f" [Pi, QasmId "q1", Tau]
          iexpr2 = Call "g" [QasmCell "q2" $ DecInt "2"]
          oexpr1 = Call "f" [Pi, QasmCell "qvar" $ Plus (DecInt "0") (DecInt "0"), Tau]
          oexpr2 = Call "g" [QasmCell "qvar" $ Plus (DecInt "2") (DecInt "1")]

test66 = TestCase (assertEqual "updateExpr handles missing declaration in QasmId."
                               (Right "qqqq" :: Either Expr String)
                               (updateExpr map7 $ QasmId "qqqq"))

test67 = TestCase (assertEqual "updateExpr handles missing declaration in QasmCell."
                               (Right "qqqq" :: Either Expr String)
                               (updateExpr map7 $ QasmCell "qqqq" $ DecInt "1"))

test68 = TestCase (assertEqual "updateExpr handles missing declaration in QVar."
                               (Right "qqq" :: Either Expr String)
                               (updateExpr map7 $ QasmMeasure $ QVar "qqq"))

test69 = TestCase (assertEqual "updateExpr handles missing declaration in QReg."
                               (Right "qqq" :: Either Expr String)
                               (updateExpr map7 $ QasmMeasure $ QReg "qqq" $ DecInt "1"))

test70 = TestCase (assertEqual "updateExpr handles invalid declaration types in QVar."
                               (Right "c2" :: Either Expr String)
                               (updateExpr map7 $ QasmMeasure $ QVar "c2"))

test71 = TestCase (assertEqual "updateExpr handles invalid declaration types in QReg."
                               (Right "c2" :: Either Expr String)
                               (updateExpr map7 $ QasmMeasure $ QReg "c2" $ DecInt "1"))

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
                                     TestLabel "lookupCVar_NoMatch" test51,
                                     TestLabel "toMergedSize_Nothing" test52,
                                     TestLabel "toMergedSize_Just" test53,
                                     TestLabel "lookupOperand_QRef" test54,
                                     TestLabel "lookupOperand_Cell" test55,
                                     TestLabel "lookupOperand_QRef_Err" test56,
                                     TestLabel "lookupOperand_Cell_Err" test57,
                                     TestLabel "lookupOperands" test58,
                                     TestLabel "lookupOperands_Err" test59,
                                     TestLabel "lookupCtorOperand" test60,
                                     TestLabel "lookupCtorOperand_Err" test61,
                                     TestLabel "updateExpr_QasmId" test62,
                                     TestLabel "updateExpr_QasmCell" test63,
                                     TestLabel "updateExpr_QasmMeasure" test64,
                                     TestLabel "updateExpr_Call" test65,
                                     TestLabel "updateExpr_Decl_Except_1" test66,
                                     TestLabel "updateExpr_Decl_Except_2" test67,
                                     TestLabel "updateExpr_Decl_Except_3" test68,
                                     TestLabel "updateExpr_Decl_Except_4" test69,
                                     TestLabel "updateExpr_Type_Except_1" test68,
                                     TestLabel "updateExpr_Type_Except_2" test69]

main = defaultMain tests
