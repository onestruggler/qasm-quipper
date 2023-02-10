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

-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
-- toAst: assignment

qdecStmt = QasmDeclStmt QubitT "qvar"

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
                                     TestLabel "toAst_Assign_Test2" test13]

main = defaultMain tests
