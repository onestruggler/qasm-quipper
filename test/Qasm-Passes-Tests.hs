-- | Simple unit tests for OpenQASM pre/post-processing passes. End-to-end
-- testing of the pipeline is left to integration tests. The purpose of this
-- test suite is to ensure that Qasm.Passes does not break in any obvious way.

module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Qasm.AST
import Qasm.Expression
import Qasm.Gate
import Qasm.GateName
import Qasm.Language
import Qasm.Passes

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
          operands = [QVar "v1", QVar "v2"]
          gateExpr = PowMod (DecInt "5") (NamedGateOp "cx" [] operands)
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
    where opLen1 = [QVar "v1"]
          opLen2 = [QVar "v1", QVar "v2"]
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
          stmt = AstGateStmt 0 (NamedGate (UserDefined name) [] [QVar "v1"] imod)

test9 = TestCase (assertEqual "elimInv returns unknown native inv errors."
                              (Right (UnknownNativeInv 1 GateU))
                              (elimInv [stmt]))
    where imod = negateMod nullGateMod
          name = "asdf"
          stmt = AstGateStmt 0 (NamedGate GateU [Pi, Pi] [QVar "v1"] imod)

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
                                     TestLabel "elimInv_NativeErr" test9]

main = defaultMain tests
