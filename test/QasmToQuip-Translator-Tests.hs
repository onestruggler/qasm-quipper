module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.AST
import LinguaQuanta.Qasm.Expression
import LinguaQuanta.Qasm.Gate as Qasm
import LinguaQuanta.Qasm.GateName as Qasm
import LinguaQuanta.Qasm.GateName
import LinguaQuanta.Qasm.Language
import LinguaQuanta.Qasm.Operand
import LinguaQuanta.QasmToQuip.Translator
import LinguaQuanta.Quip.Gate as Quip
import LinguaQuanta.Quip.GateName as Quip
import LinguaQuanta.Quip.Quipper
import LinguaQuanta.Quip.Wire

import qualified Data.IntMap.Strict as IntMap

-----------------------------------------------------------------------------------------
-- Basic QWire Tests.

dstmt1 = AstQubitDecl Nothing  "decl1"
dstmt2 = AstQubitDecl (Just 4) "decl1"
dstmt3 = AstQubitDecl (Nothing) "decl2"

circ1 = translate []
circ2 = translate [dstmt1]
circ3 = translate [dstmt2]
circ4 = translate [dstmt2, dstmt3]

test1 = TestCase (assertEqual "translate (inputs): empty circuit."
                              IntMap.empty
                              (inputs circ1))

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
                              (inputs circ2))

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
                              (inputs circ3))

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
                               (inputs circ4))

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
-- Translating Unitaries

getOp :: Int -> Qasm.Operand
getOp 4 = QRef "decl2"
getOp n = Cell "decl1" n

mod0 = Qasm.nullGateMod
mod1 = Qasm.negateMod mod0
mod2 = Qasm.addCtrlsToMod 2 $ mod1
mod3 = Qasm.addNegCtrlsToMod 1 $ addCtrlsToMod 2 $ mod0

gate1 = Qasm.NamedGate Qasm.GateCCX [] [getOp 0, getOp 1, getOp 2, getOp 3, getOp 4] mod2
gate2 = Qasm.NamedGate Qasm.GateSdg [] [getOp 2] mod1
gate3 = Qasm.GPhaseGate Pi [getOp 0, getOp 1, getOp 2] mod3

gstmt1 = AstGateStmt 0 gate1
gstmt2 = AstGateStmt 1 gate3
gstmt3 = AstGateStmt 2 gate2
gstmt4 = AstGateStmt 3 gate1

qgate1 = Quip.NamedGate Quip.GateX False [4] $ map Quip.Pos [2, 3, 0, 1]
qgate2 = Quip.PhaseGate 1.0 [Quip.Neg 0, Quip.Pos 1, Quip.Pos 2]
qgate3 = Quip.NamedGate Quip.GateS False [2] []

circ5 = translate [dstmt2, dstmt3, gstmt1]
circ6 = translate [dstmt2, gstmt2]
circ7 = translate [dstmt2, dstmt3, gstmt3, gstmt2, gstmt4]

test17 = TestCase (assertEqual "translate (inputs): isolated CCX gate."
                               circ4_io
                               (inputs circ5))

test18 = TestCase (assertEqual "translate (gates): isolated named gate."
                               [qgate1]
                               (gates circ5))

test19 = TestCase (assertEqual "translate (outputs): isolated named gate."
                               circ4_io
                               (outputs circ5))

test20 = TestCase (assertEqual "translate (size): isolated named gate."
                               5
                               (size circ5))

test21 = TestCase (assertEqual "translate (inputs): isolated gphase gate."
                               circ3_io
                               (inputs circ6))

test22 = TestCase (assertEqual "translate (gates): isolated gphase gate."
                               [qgate2]
                               (gates circ6))

test23 = TestCase (assertEqual "translate (outputs): isolated gphase gate."
                               circ3_io
                               (outputs circ6))

test24 = TestCase (assertEqual "translate (size): isolated gphase gate."
                               4
                               (size circ6))

test25 = TestCase (assertEqual "translate (inputs): mixed unitary gates."
                               circ4_io
                               (inputs circ7))

test26 = TestCase (assertEqual "translate (gates): mixed unitary gates."
                               [qgate3, qgate3, qgate2, qgate1, qgate1, qgate1]
                               (gates circ7))

test27 = TestCase (assertEqual "translate (outputs): mixed unitary gates."
                               circ4_io
                               (outputs circ7))

test28 = TestCase (assertEqual "translate (size): mixed unitary gates."
                               5
                               (size circ7))

-----------------------------------------------------------------------------------------
-- Rotational Tests

p1 = DecFloat "1.0"
p2 = DecFloat "2.0"
p3 = DecFloat "3.0"

gate4 = Qasm.NamedGate Qasm.GateRZ [p1] [getOp 0, getOp 1, getOp 3] mod2
gate5 = Qasm.NamedGate Qasm.GateU2 [p2, p3] [getOp 0] mod0
gate6 = Qasm.NamedGate Qasm.GateU3 [p1, p2, p3] [getOp 0] mod0

gstmt5 = AstGateStmt 1 gate4
gstmt6 = AstGateStmt 1 gate5
gstmt7 = AstGateStmt 1 gate6

circ8 = translate [dstmt2, dstmt3, gstmt5, gstmt6, gstmt7]

test29 = TestCase (assertEqual "translate (inputs): miscellaneous rotations."
                               circ4_io
                               (inputs circ8))

test30 = case toConstFloat $ Div p1 Pi of
    Right err -> TestCase (assertFailure "Unable to parse p1.")
    Left ph   -> case toConstFloat $ Div p1 $ DecInt "2" of
        Right err  -> TestCase (assertFailure "Unable to parse p1.")
        Left t1    -> case toConstFloat $ Div a1 $ DecInt "2" of
            Right err -> TestCase (assertFailure "Unable to parse p1.")
            Left t2   -> case toConstFloat $ Div a2 $ DecInt "2" of
                Right err -> TestCase (assertFailure "Unable to parse p3.")
                Left t3   -> let circ = [Quip.RotGate   Quip.RotExpZ   True  0.5 [3] ctrls,
                                         Quip.NamedGate Quip.GateOmega False     [0] [],
                                         Quip.RotGate   Quip.RotExpZ   False t2  [0] [],
                                         Quip.NamedGate Quip.GateH     False     [0] [],
                                         Quip.NamedGate Quip.GateS     False     [0] [],
                                         Quip.NamedGate Quip.GateH     False     [0] [],
                                         Quip.RotGate   Quip.RotExpZ   False t3  [0] [],
                                         Quip.NamedGate Quip.GateOmega False     [0] [],
                                         Quip.NamedGate Quip.GateOmega False     [0] [],
                                         Quip.PhaseGate                      ph      [],
                                         Quip.RotGate   Quip.RotExpZ   False t1  [0] [],
                                         Quip.NamedGate Quip.GateH     False     [0] [],
                                         Quip.RotGate   Quip.RotExpZ   False t2  [0] [],
                                         Quip.NamedGate Quip.GateH     False     [0] [],
                                         Quip.RotGate   Quip.RotExpZ   False t3  [0] []]
                      in TestCase (assertEqual msg circ $ gates circ8)
    where msg   = "translate (gates): miscellaneous rotations."
          a1    = Plus p2 $ Div Pi $ DecInt "2"
          a2    = Minus p3 $ Div Pi $ DecInt "2"
          ctrls = [Quip.Pos 0, Quip.Pos 1]

test31 = TestCase (assertEqual "translate (outputs): miscellaneous rotations."
                               circ4_io
                               (outputs circ8))

test32 = TestCase (assertEqual "translate (size): miscellaneous rotations."
                               5
                               (size circ8))

-----------------------------------------------------------------------------------------
-- Basic CWire Tests.

dstmt4 = AstBitDecl Nothing  "decl3"
dstmt5 = AstBitDecl (Just 4) "decl3"
dstmt6 = AstBitDecl (Nothing) "decl4"

circ9  = translate [dstmt4]
circ10 = translate [dstmt5]
circ11 = translate [dstmt5, dstmt6]

circ9_io = IntMap.fromList [(0, CWire)]

test33 = TestCase (assertEqual "translate (inputs): single cbit."
                               circ9_io
                               (inputs circ9))

test34 = TestCase (assertEqual "translate (gates): single cbit."
                               []
                               (gates circ9))

test35 = TestCase (assertEqual "translate (outputs): single cbit."
                               circ9_io
                               (outputs circ9))

test36 = TestCase (assertEqual "translate (size): single cbit."
                               1
                               (size circ9))


circ10_io = IntMap.fromList [(0, CWire), (1, CWire), (2, CWire), (3, CWire)]

test37 = TestCase (assertEqual "translate (inputs): cbit array."
                               circ10_io
                               (inputs circ10))

test38 = TestCase (assertEqual "translate (gates): cbit array."
                               []
                               (gates circ10))

test39 = TestCase (assertEqual "translate (outputs): cbit array."
                               circ10_io
                               (outputs circ10))

test40 = TestCase (assertEqual "translate (size): cbit array."
                               4
                               (size circ10))

circ11_io = IntMap.fromList [(0, CWire), (1, CWire), (2, CWire), (3, CWire), (4, CWire)]

test41 = TestCase (assertEqual "translate (inputs): cbit array and single cbit."
                               circ11_io
                               (inputs circ11))

test42 = TestCase (assertEqual "translate (gates): cbit array and single cbit."
                               []
                               (gates circ11))

test43 = TestCase (assertEqual "translate (outputs): cbit array and single cbit."
                               circ11_io
                               (outputs circ11))

test44 = TestCase (assertEqual "translate (size): cbit array and single cbit."
                               5
                               (size circ11))

-----------------------------------------------------------------------------------------
-- Mixed QWire and CWire Tests.

circ12 = translate [dstmt2, dstmt5, dstmt3, dstmt6]

circ12_io = IntMap.fromList [(0, QWire), (1, QWire), (2, QWire), (3, QWire),
                             (4, CWire), (5, CWire), (6, CWire), (7, CWire),
                             (8, QWire),
                             (9, CWire)]

test45 = TestCase (assertEqual "translate (inputs): mixed qbit and cbit."
                               circ12_io
                               (inputs circ12))

test46 = TestCase (assertEqual "translate (gates): mixed qbit and cbit."
                               []
                               (gates circ12))

test47 = TestCase (assertEqual "translate (outputs): mixed qbit and cbit."
                               circ12_io
                               (outputs circ12))

test48 = TestCase (assertEqual "translate (size): mixed qbit and cbit."
                               10
                               (size circ12))

-----------------------------------------------------------------------------------------
-- Inverse classical ancilla translation.

circ13 = translate [AstBitDecl (Just 20) "cvars",
                    AstAssign "cvars" (Just 2) QuipCInit0,
                    AstAssign "cvars" (Just 4) QuipCInit1,
                    AstAssign "cvars" (Just 5) QuipCTerm0,
                    AstAssign "cvars" (Just 4) QuipCTerm1,
                    AstAssign "cvars" (Just 10) QuipCDiscard]

test49 = TestCase (assertEqual "translate (inputs): classical ancillas."
                               18
                               (IntMap.size $ inputs circ13))

test50 = TestCase (assertEqual "translate (gates): classical ancillas."
                               [CInitGate False 2,
                                CInitGate True 4,
                                CTermGate False 5,
                                CTermGate True 4,
                                CDiscardGate 10]
                               (gates circ13))

test51 = TestCase (assertEqual "translate (outputs): classical ancillas."
                               17
                               (IntMap.size $ outputs circ13))

test52 = TestCase (assertEqual "translate (size): classical ancillas."
                               20
                               (size circ13))

-----------------------------------------------------------------------------------------
-- Inverse quantum ancilla translation.

circ14 = translate [AstQubitDecl (Just 20) "qvars",
                    AstCall $ QuipQInit0 $ Cell "qvars" 2,
                    AstCall $ QuipQInit1 $ Cell "qvars" 4,
                    AstCall $ QuipQTerm0 $ Cell "qvars" 5,
                    AstCall $ QuipQTerm1 $ Cell "qvars" 4,
                    AstCall $ QuipQDiscard $ Cell "qvars" 10]

test53 = TestCase (assertEqual "translate (inputs): quantum ancillas."
                               18
                               (IntMap.size $ inputs circ14))

test54 = TestCase (assertEqual "translate (gates): quantum ancillas."
                               [QInitGate False 2,
                                QInitGate True 4,
                                QTermGate False 5,
                                QTermGate True 4,
                                QDiscardGate 10]
                               (gates circ14))

test55 = TestCase (assertEqual "translate (outputs): quantum ancillas."
                               17
                               (IntMap.size $ outputs circ14))

test56 = TestCase (assertEqual "translate (size): quantum ancillas."
                               20
                               (size circ14))

-----------------------------------------------------------------------------------------
-- Measurement translations.

circ15 = translate [AstQubitDecl Nothing "qvar",
                    AstBitDecl Nothing "cvar",
                    AstAssign "cvar" Nothing $ Measure $ QRef "qvar"]

test57 = TestCase (assertEqual "translate (inputs): measure and assign."
                               2
                               (IntMap.size $ inputs circ15))

test58 = TestCase (assertEqual "translate (gates): measure and assign."
                               [CDiscardGate 1,
                                QInitGate False 1,
                                Quip.NamedGate Quip.GateX False [1] [Quip.Pos 0],
                                QMeasGate 1]
                               (gates circ15))

test59 = TestCase (assertEqual "translate (outputs): measure and assign."
                               2
                               (IntMap.size $ outputs circ15))

test60 = TestCase (assertEqual "translate (size): measure and assign."
                               2
                               (size circ15))

-----------------------------------------------------------------------------------------
-- * Void measure translations.

circ16 = translate [AstQubitDecl Nothing "qvar",
                    AstQubitDecl (Just 5) "qreg",
                    AstCall $ VoidMeasure $ QRef "qvar",
                    AstCall $ VoidMeasure $ Cell "qreg" 2]

test61 = TestCase (assertEqual "translate (inputs): void measure."
                               6
                               (IntMap.size $ inputs circ16))

test62 = TestCase (assertEqual "translate (gates): void measure."
                               [QInitGate False 6,
                                Quip.NamedGate Quip.GateX False [6] [Quip.Pos 0],
                                QMeasGate 6,
                                CDiscardGate 6,
                                QInitGate False 7,
                                Quip.NamedGate Quip.GateX False [7] [Quip.Pos 3],
                                QMeasGate 7,
                                CDiscardGate 7]
                               (gates circ16))

test63 = TestCase (assertEqual "translate (outputs): void measure."
                               6
                               (IntMap.size $ outputs circ16))

test64 = TestCase (assertEqual "translate (size): void measure."
                               7
                               (size circ16))

-----------------------------------------------------------------------------------------
-- Measurement translations.

circ17 = translate [AstQubitDecl Nothing "qvar",
                    AstBitDecl Nothing "cvar",
                    AstAssign "cvar" Nothing $ QuipMeasure $ QRef "qvar"]

test65 = TestCase (assertEqual "translate (inputs): measure and assign."
                               1
                               (IntMap.size $ inputs circ17))

test66 = TestCase (assertEqual "translate (gates): measure and assign."
                               [QMeasGate 0]
                               (gates circ17))

test67 = TestCase (assertEqual "translate (outputs): measure and assign."
                               1
                               (IntMap.size $ outputs circ17))

test68 = TestCase (assertEqual "translate (size): measure and assign."
                               1
                               (size circ17))

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
                                     TestLabel "1Qarr1Qbit_Size" test16,
                                     TestLabel "CCX_Inputs" test17,
                                     TestLabel "CCX_Gates" test18,
                                     TestLabel "CCX_Outputs" test19,
                                     TestLabel "CCX_Size" test20,
                                     TestLabel "GPhase_Inputs" test21,
                                     TestLabel "GPhase_Gates" test22,
                                     TestLabel "GPhase_Outputs" test23,
                                     TestLabel "GPhase_Size" test24,
                                     TestLabel "MixedUnitary_Inputs" test25,
                                     TestLabel "MixedUnitary_Gates" test26,
                                     TestLabel "MixedUnitary_Outputs" test27,
                                     TestLabel "MixedUnitary_Size" test28,
                                     TestLabel "MixedRot_Inputs" test29,
                                     TestLabel "MixedRot_Gates" test30,
                                     TestLabel "MixedRot_Outputs" test31,
                                     TestLabel "MixedRot_Size" test32,
                                     TestLabel "1Cbit_Inputs" test33,
                                     TestLabel "1Cbit_Gates" test34,
                                     TestLabel "1Cbit_Outputs" test35,
                                     TestLabel "1Cbit_Size" test36,
                                     TestLabel "1Carr_Inputs" test37,
                                     TestLabel "1Carr_Gates" test38,
                                     TestLabel "1Carr_Outputs" test39,
                                     TestLabel "1Carr_Size" test40,
                                     TestLabel "1Carr1Cbit_Inputs" test41,
                                     TestLabel "1Carr1Cbit_Gates" test42,
                                     TestLabel "1Carr1Cbit_Outputs" test43,
                                     TestLabel "1Carr1Cbit_Size" test44,
                                     TestLabel "Mixed_Inputs" test45,
                                     TestLabel "Mixed_Gates" test46,
                                     TestLabel "Mixed_Outputs" test47,
                                     TestLabel "Mixed_Size" test48,
                                     TestLabel "CAncilla_Inputs" test49,
                                     TestLabel "CAncilla_Gates" test50,
                                     TestLabel "CAncilla_Outputs" test51,
                                     TestLabel "CAncilla_Size" test52,
                                     TestLabel "QAncilla_Inputs" test53,
                                     TestLabel "QAncilla_Gates" test54,
                                     TestLabel "QAncilla_Outputs" test55,
                                     TestLabel "QAncilla_Size" test56,
                                     TestLabel "QMeas_Assign_Inputs" test57,
                                     TestLabel "QMeas_Assign_Gates" test58,
                                     TestLabel "QMeas_Assign_Outputs" test59,
                                     TestLabel "QMeas_Assign_Size" test60,
                                     TestLabel "QMeas_Void_Inputs" test61,
                                     TestLabel "QMeas_Void_Gates" test62,
                                     TestLabel "QMeas_Void_Outputs" test63,
                                     TestLabel "QMeas_Void_Size" test64,
                                     TestLabel "InvQMeas_Inputs" test65,
                                     TestLabel "InvQMeas_Gates" test66,
                                     TestLabel "InvQMeas_Outputs" test67,
                                     TestLabel "InvQMeas_Size" test68]

main = defaultMain tests
