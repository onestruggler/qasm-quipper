-- | Simple unit tests for OpenQASM pre/post-processing passes. End-to-end
-- testing of the pipeline is left to integration tests. The purpose of this
-- test suite is to ensure that Qasm.Passes does not break in any obvious way.

module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.AST
import LinguaQuanta.Qasm.Expression
import LinguaQuanta.Qasm.Gate
import LinguaQuanta.Qasm.GateName
import LinguaQuanta.Qasm.Language
import LinguaQuanta.Qasm.Operand
import LinguaQuanta.Qasm.Passes

-----------------------------------------------------------------------------------------
-- toAst

test1 = TestCase (assertEqual "toAst supports empty files."
                              (Left [])
                              (toAst []))

test2 = TestCase (assertEqual "toAst supports basic features."
                              (Left [astDecl1, astDecl2, astDecl3, astGate1])
                              (toAst [dec1Stmt, dec2Stmt, dec3Stmt, gateStmt]))
    where dec1Stmt = QasmDeclStmt QubitT "v1"
          dec2Stmt = QasmDeclStmt QubitT "v2"
          dec3Stmt = QasmDeclStmt (QubitArrT (DecInt "7")) "arr"
          preparse = [QVar "v1", QVar "v2"]
          operands = [QRef "v1", QRef "v2"]
          gateExpr = PowMod (DecInt "5") (NamedGateOp "cx" [] preparse)
          gateStmt = QasmGateStmt gateExpr
          astDecl1 = AstQubitDecl Nothing "v1"
          astDecl2 = AstQubitDecl Nothing "v2"
          astDecl3 = AstQubitDecl (Just 7) "arr"
          astGate1 = AstGateStmt 5 (NamedGate GateCX [] operands nullGateMod)

test3 = TestCase (assertEqual "toAst returns gate errors."
                              (Right (GateAbstractionErr 2 (UnexpectedParamCount 1 0)))
                              (toAst [declStmt, gateStmt]))
    where declStmt = QasmDeclStmt QubitT "v1"
          gateExpr = NamedGateOp "cx" [Pi] [QVar "v1", QVar "v2", QVar "v3"]
          gateStmt = QasmGateStmt gateExpr

test4 = TestCase (assertEqual "toAst returns array length errors (non-integral)."
                              (Right (ArrayLenAbstractionErr 1 (BadType "angle")))
                              (toAst [QasmDeclStmt (QubitArrT Pi) "arr"]))

test5 = TestCase (assertEqual "toAst returns array length errors (non-positive)."
                              (Right (NonPosArrayLen 1))
                              (toAst [QasmDeclStmt (QubitArrT zero) "arr"]))

-----------------------------------------------------------------------------------------
-- elimInv

test6 = TestCase (assertEqual "elimInv supports empty files."
                              (Left [])
                              (elimInv []))

test7 = TestCase (assertEqual "elimInv supports basic features."
                              (Left ostmts)
                              (elimInv istmts))
    where opLen1 = [QRef "v1"]
          opLen2 = [QRef "v1", QRef "v2"]
          d1Stmt = AstQubitDecl Nothing "v1"
          d2Stmt = AstQubitDecl Nothing "v2"
          invMod = negateMod nullGateMod
          g1Stmt = AstGateStmt 5 (NamedGate GateCX [] opLen2 nullGateMod)
          g2Stmt = AstGateStmt 0 (NamedGate GateS [] opLen1 invMod)
          i2Stmt = AstGateStmt 0 (NamedGate GateSdg [] opLen1 nullGateMod)
          g3Stmt = AstGateStmt 1 (NamedGate GateTdg [] opLen1 invMod)
          i3Stmt = AstGateStmt 1 (NamedGate GateT [] opLen1 nullGateMod)
          g4Stmt = AstGateStmt 2 (NamedGate GateX [] opLen1 invMod)
          i4Stmt = AstGateStmt 2 (NamedGate GateX [] opLen1 nullGateMod)
          g5Stmt = AstGateStmt 2 (NamedGate GateSX [] opLen1 invMod)
          i5List = [AstGateStmt 0 (NamedGate GateSX [] opLen1 nullGateMod),
                    AstGateStmt 0 (NamedGate GateX [] opLen1 nullGateMod),
                    AstGateStmt 0 (NamedGate GateSX [] opLen1 nullGateMod),
                    AstGateStmt 0 (NamedGate GateX [] opLen1 nullGateMod)]
          istmts = [d1Stmt, d2Stmt, g1Stmt, g2Stmt, g3Stmt, g4Stmt, g5Stmt]
          ostmts = [d1Stmt, d2Stmt, g1Stmt, i2Stmt, i3Stmt, i4Stmt] ++ i5List


test8 = TestCase (assertEqual "elimInv returns unknown user-defined inv errors."
                              (Right (UnknownUserDefinedInv 1 name))
                              (elimInv [stmt]))
    where imod = negateMod nullGateMod
          name = "asdf"
          stmt = AstGateStmt 0 (NamedGate (UserDefined name) [] [QRef "v1"] imod)

test9 = TestCase (assertEqual "elimInv returns unknown native inv errors."
                              (Right (UnknownNativeInv 1 GateU))
                              (elimInv [stmt]))
    where imod = negateMod nullGateMod
          name = "asdf"
          stmt = AstGateStmt 0 (NamedGate GateU [Pi, Pi] [QRef "v1"] imod)

-----------------------------------------------------------------------------------------
-- elimPow

test10 = TestCase (assertEqual "elimPow supports empty files."
                               []
                               (elimPow []))

test11 = TestCase (assertEqual "elimPow supports empty files."
                               [decl1, decl2, elim1, elim1, elim1, elim2, elim3]
                               (elimPow [decl1, decl2, stmt1, stmt2, stmt3]))
    where opLn1 = [QRef "v1"]
          opLn2 = [QRef "v1", QRef "v2"]
          decl1 = AstQubitDecl Nothing "v1"
          decl2 = AstQubitDecl Nothing "v2"
          gate1 = NamedGate GateCX [] opLn2 nullGateMod
          gate2 = NamedGate GateS [] opLn1 nullGateMod
          gate3 = NamedGate GateTdg [] opLn1 nullGateMod
          stmt1 = AstGateStmt 3 gate1
          stmt2 = AstGateStmt 0 gate2
          stmt3 = AstGateStmt 1 gate3
          elim1 = AstGateStmt 0 gate1
          elim2 = AstGateStmt 0 gate2
          elim3 = AstGateStmt 0 gate3

-----------------------------------------------------------------------------------------
-- toAst: assignment

qdecStmt = QasmDeclStmt QubitT "qvar"
cdecStmt = QasmDeclStmt BitT   "cvar"

cdecAst = AstBitDecl Nothing "cvar"
qdecAst = AstQubitDecl Nothing "qvar"
asgnAst = AstAssign "cvar" Nothing $ QuipMeasure $ QRef "qvar"

test12 = TestCase (assertEqual "toAst supports QasmAssignStmt."
                               (Left [qdecAst, cdecAst, asgnAst])
                               (toAst [qdecStmt, cdecStmt, asgnStmt]))
    where asgnStmt = QasmAssignStmt (CVar "cvar") $ Call "QMeas" [QasmId "qvar"]
          cdecStmt = QasmDeclStmt BitT "cvar"

test13 = TestCase (assertEqual "toAst supports QasmInitDeclStmt."
                               (Left [qdecAst, cdecAst, asgnAst])
                               (toAst [qdecStmt, asgnStmt]))
    where asgnStmt = QasmInitDeclStmt BitT "cvar" $  Call "QMeas" [QasmId "qvar"]

-----------------------------------------------------------------------------------------
-- toAst: expression statements

qterm0   = QasmExprStmt $ Call "QTerm0"   [QasmId "qvar"]
qterm1   = QasmExprStmt $ Call "QTerm1"   [QasmId "qvar"]
qinit0   = QasmExprStmt $ Call "QInit0"   [QasmId "qvar"]
qinit1   = QasmExprStmt $ Call "QInit1"   [QasmId "qvar"]
qdiscard = QasmExprStmt $ Call "QDiscard" [QasmId "qvar"]

qterm0Ast   = AstCall $ QuipQTerm0   $ QRef "qvar"
qterm1Ast   = AstCall $ QuipQTerm1   $ QRef "qvar"
qinit0Ast   = AstCall $ QuipQInit0   $ QRef "qvar"
qinit1Ast   = AstCall $ QuipQInit1   $ QRef "qvar"
qdiscardAst = AstCall $ QuipQDiscard $ QRef "qvar"

test14 = TestCase (assertEqual "toAst supports ancilla qbit function calls."
                               (Left ast)
                               (toAst prog))
    where prog = [qdecStmt, qterm0,    qinit1,    qterm1,    qinit0,    qdiscard]
          ast  = [qdecAst,  qterm0Ast, qinit1Ast, qterm1Ast, qinit0Ast, qdiscardAst]

test15 = TestCase (assertEqual "toAst rejects invalid ancilla qbit function calls."
                               (Right err :: Either [AstStmt] AbstractionErr)
                               (toAst [qdecStmt, QasmExprStmt $ Call "QInit0" []]))
    where err = VoidCallAbstractionErr 2 $ CallArityMismatch "QInit0" 0 1

cterm0   = QasmAssignStmt (CVar "cvar") $ Call "CTerm0"   []
cterm1   = QasmAssignStmt (CVar "cvar") $ Call "CTerm1"   []
cinit0   = QasmAssignStmt (CVar "cvar") $ Call "CInit0"   []
cinit1   = QasmAssignStmt (CVar "cvar") $ Call "CInit1"   []
cdiscard = QasmAssignStmt (CVar "cvar") $ Call "CDiscard" []

cterm0Ast   = AstAssign "cvar" Nothing QuipCTerm0
cterm1Ast   = AstAssign "cvar" Nothing QuipCTerm1
cinit0Ast   = AstAssign "cvar" Nothing QuipCInit0
cinit1Ast   = AstAssign "cvar" Nothing QuipCInit1
cdiscardAst = AstAssign "cvar" Nothing QuipCDiscard

test16 = TestCase (assertEqual "toAst supports ancilla cbit function calls."
                              (Left ast)
                              (toAst prog))
    where prog = [cdecStmt, cterm0,    cinit1,    cterm1,    cinit0,    cdiscard]
          ast  = [cdecAst,  cterm0Ast, cinit1Ast, cterm1Ast, cinit0Ast, cdiscardAst]

test17 = TestCase (assertEqual "toAst rejects invalid ancilla cbit function calls."
                               (Right err :: Either [AstStmt] AbstractionErr)
                               (toAst [cdecStmt, stmt]))
    where stmt = QasmAssignStmt (CVar "cvar") $ Call "CTerm0" [QasmId "qvar"]
          err  = RValueAbstractionErr 2 $ CallArityMismatch "CTerm0" 1 0

test18 = TestCase (assertEqual "toAst strips outer brackets from expressions.."
                               (Left ast :: Either [AstStmt] AbstractionErr)
                               (toAst [cdecStmt, stmt]))
    where stmt = QasmExprStmt $ Brack $ Call "QTerm0" [QasmId "qvar"]
          ast  = [cdecAst,  qterm0Ast]

-----------------------------------------------------------------------------------------
-- Measurement and reset calls.

resetStmt    = QasmResetStmt $ QVar "qvar"
measure1Stmt = QasmAssignStmt (CVar "cvar") $ QasmMeasure $ QVar "qvar"
measure2Stmt = QasmExprStmt $ QasmMeasure $ QVar "qvar"

resetAst    = AstCall $ VoidReset $ QRef "qvar"
measure1Ast = AstAssign "cvar" Nothing $ Measure $ QRef "qvar"
measure2Ast = AstCall $ VoidMeasure $ QRef "qvar"

test19 = TestCase (assertEqual "toAst supports reset/measure."
                              (Left ast)
                              (toAst prog))
    where prog = [cdecStmt, qdecStmt, measure1Stmt, measure2Stmt, resetStmt]
          ast  = [cdecAst,  qdecAst,  measure1Ast,  measure2Ast,  resetAst]

test20 = TestCase (assertEqual "toAst rejects invalid reset operands."
                               (Right err :: Either [AstStmt] AbstractionErr)
                               (toAst [cdecStmt, qdecStmt, stmt]))
    where stmt = QasmResetStmt $ QReg "qvar" $ DecInt "-5"
          err  = MeasureCallAbstractionErr 3 $ NegArrIdx (-5)

test21 = TestCase (assertEqual "toAst rejects invalid measure in assignment."
                               (Right err :: Either [AstStmt] AbstractionErr)
                               (toAst [cdecStmt, qdecStmt, stmt]))
    where stmt = QasmAssignStmt (CVar "cvar") $ QasmMeasure $ QReg "qvar" $ DecInt "-5"
          err  = RValueAbstractionErr 3 $ NegArrIdx (-5)

test22 = TestCase (assertEqual "toAst rejects invalid measure in QasmExprStmt."
                               (Right err :: Either [AstStmt] AbstractionErr)
                               (toAst [cdecStmt, qdecStmt, stmt]))
    where stmt = QasmExprStmt $ QasmMeasure $ QReg "qvar" $ DecInt "-5"
          err  = MeasureCallAbstractionErr 3 $ NegArrIdx (-5)

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "toAst_EmptyFile" test1,
                                     TestLabel "toAst_Basic" test2,
                                     TestLabel "toAst_GateErr" test3,
                                     TestLabel "toAst_ArrLenErr_NonInt" test4,
                                     TestLabel "toAst_ArrLenErr_NonPos" test5,
                                     TestLabel "elimInv_EmptyFile" test6,
                                     TestLabel "elimInv_Basic" test7,
                                     TestLabel "elimInv_UserDefinedErr" test8,
                                     TestLabel "elimInv_NativeErr" test9,
                                     TestLabel "elimPow_EmptyFile" test10,
                                     TestLabel "elimPow_Basic" test11,
                                     TestLabel "toAst_Assign_Test1" test12,
                                     TestLabel "toAst_Assign_Test2" test13,
                                     TestLabel "toAst_Quantum_Ancilla" test14,
                                     TestLabel "toAst_Quantum_Ancilla_Err" test15,
                                     TestLabel "toAst_Classical_Ancilla" test16,
                                     TestLabel "toAst_Classical_Ancilla_Err" test17,
                                     TestLabel "toAst_ExprStmt_Brackets" test18,
                                     TestLabel "toAst_MeasurementReset" test19,
                                     TestLabel "toAst_BadReset" test20,
                                     TestLabel "toAst_BadMeasureAssign" test21,
                                     TestLabel "toAst_BadMeasureExpr" test22]

main = defaultMain tests
