-- | Simple unit tests for OpenQASM pre/post-processing passes. End-to-end
-- testing of the pipeline is left to integration tests. The purpose of this
-- test suite is to ensure that Qasm.Passes does not break in any obvious way.

module Main where

import Data.Either
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.AST
import LinguaQuanta.Qasm.Expression
import LinguaQuanta.Qasm.Gate
import LinguaQuanta.Qasm.GateName
import LinguaQuanta.Qasm.Header
import LinguaQuanta.Qasm.Language
import LinguaQuanta.Qasm.LatticeSurgery
import LinguaQuanta.Qasm.Operand
import LinguaQuanta.Qasm.Passes

-----------------------------------------------------------------------------------------
-- Computes the most permissive header.

(Just defhdr) = toQasmHeader "3"
(Left stdhdr) = addLib "stdgates.inc" defhdr
(Left tmp2)   = addLib "quipgates.inc" stdhdr
(Left libhdr) = addLib "quipfuncs.inc" tmp2

-----------------------------------------------------------------------------------------
-- toAst

test1 = TestCase (assertEqual "toAst supports empty files."
                              (Left [])
                              (toAst libhdr []))

test2 = TestCase (assertEqual "toAst supports basic features."
                              (Left [astDecl1, astDecl2, astDecl3, astGate1])
                              (toAst libhdr [dec1Stmt, dec2Stmt, dec3Stmt, gateStmt]))
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
                              (toAst libhdr [declStmt, gateStmt]))
    where declStmt = QasmDeclStmt QubitT "v1"
          gateExpr = NamedGateOp "cx" [Pi] [QVar "v1", QVar "v2", QVar "v3"]
          gateStmt = QasmGateStmt gateExpr

test4 = TestCase (assertEqual "toAst returns array length errors (non-integral)."
                              (Right (ArrayLenAbstractionErr 1 (BadType "angle")))
                              (toAst libhdr [QasmDeclStmt (QubitArrT Pi) "arr"]))

test5 = TestCase (assertEqual "toAst returns array length errors (non-positive)."
                              (Right (NonPosArrayLen 1))
                              (toAst libhdr [QasmDeclStmt (QubitArrT zero) "arr"]))

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
                               (toAst libhdr [qdecStmt, cdecStmt, asgnStmt]))
    where asgnStmt = QasmAssignStmt (CVar "cvar") $ Call "QMeas" [QasmId "qvar"]

test13 = TestCase (assertEqual "toAst supports QasmInitDeclStmt."
                               (Left [qdecAst, cdecAst, asgnAst])
                               (toAst libhdr [qdecStmt, asgnStmt]))
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
                               (toAst libhdr prog))
    where prog = [qdecStmt, qterm0,    qinit1,    qterm1,    qinit0,    qdiscard]
          ast  = [qdecAst,  qterm0Ast, qinit1Ast, qterm1Ast, qinit0Ast, qdiscardAst]

test15 = TestCase (assertEqual "toAst rejects invalid ancilla qbit function calls."
                               (Right err :: Either [AstStmt] AbstractionErr)
                               (toAst libhdr [qdecStmt, QasmExprStmt $ Call "QInit0" []]))
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
                              (toAst libhdr prog))
    where prog = [cdecStmt, cterm0,    cinit1,    cterm1,    cinit0,    cdiscard]
          ast  = [cdecAst,  cterm0Ast, cinit1Ast, cterm1Ast, cinit0Ast, cdiscardAst]

test17 = TestCase (assertEqual "toAst rejects invalid ancilla cbit function calls."
                               (Right err :: Either [AstStmt] AbstractionErr)
                               (toAst libhdr [cdecStmt, stmt]))
    where stmt = QasmAssignStmt (CVar "cvar") $ Call "CTerm0" [QasmId "qvar"]
          err  = RValueAbstractionErr 2 $ CallArityMismatch "CTerm0" 1 0

test18 = TestCase (assertEqual "toAst strips outer brackets from expressions.."
                               (Left ast :: Either [AstStmt] AbstractionErr)
                               (toAst libhdr [cdecStmt, stmt]))
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
                              (toAst libhdr prog))
    where prog = [cdecStmt, qdecStmt, measure1Stmt, measure2Stmt, resetStmt]
          ast  = [cdecAst,  qdecAst,  measure1Ast,  measure2Ast,  resetAst]

test20 = TestCase (assertEqual "toAst rejects invalid reset operands."
                               (Right err :: Either [AstStmt] AbstractionErr)
                               (toAst libhdr [cdecStmt, qdecStmt, stmt]))
    where stmt = QasmResetStmt $ QReg "qvar" $ DecInt "-5"
          err  = MeasureCallAbstractionErr 3 $ NegArrIdx (-5)

test21 = TestCase (assertEqual "toAst rejects invalid measure in assignment."
                               (Right err :: Either [AstStmt] AbstractionErr)
                               (toAst libhdr [cdecStmt, qdecStmt, stmt]))
    where stmt = QasmAssignStmt (CVar "cvar") $ QasmMeasure $ QReg "qvar" $ DecInt "-5"
          err  = RValueAbstractionErr 3 $ NegArrIdx (-5)

test22 = TestCase (assertEqual "toAst rejects invalid measure in QasmExprStmt."
                               (Right err :: Either [AstStmt] AbstractionErr)
                               (toAst libhdr [cdecStmt, qdecStmt, stmt]))
    where stmt = QasmExprStmt $ QasmMeasure $ QReg "qvar" $ DecInt "-5"
          err  = MeasureCallAbstractionErr 3 $ NegArrIdx (-5)

-----------------------------------------------------------------------------------------
-- Legacy statement checks.

(Just defLegacy) = toQasmHeader "2.0"
(Left tmp4)      = addLib "qelib1.inc" defLegacy
(Left legacy)    = addLib "quipgates.inc" tmp4
(Left legacyAll) = addLib "bkpgates.inc" legacy

qldecStmt = QasmLDeclStmt QubitT "qvar"
cldecStmt = QasmLDeclStmt BitT   "cvar"
lmeasStmt = QasmLAssignStmt (CVar "cvar") $ QasmMeasure $ QVar "qvar"

test23 = TestCase (assertEqual "toAst rejects new declarations in legacy code (1/2)."
                               (Right err :: Either [AstStmt] AbstractionErr)
                               (toAst legacy [cdecStmt]))
    where err = NonLegacyStmt 1

test24 = TestCase (assertEqual "toAst rejects new declarations in legacy code (2/2)."
                               (Left [cdecAst] :: Either [AstStmt] AbstractionErr)
                               (toAst legacy [cldecStmt]))

test25 = TestCase (assertEqual "toAst rejects new assignments in legacy code (1/2)."
                               (Right err :: Either [AstStmt] AbstractionErr)
                               (toAst legacy [cldecStmt, qldecStmt, asgnStmt]))
    where asgnStmt = QasmAssignStmt (CVar "cvar") $ Call "QMeas" [QasmId "qvar"]
          err      = NonLegacyStmt 3

test26 = TestCase (assertEqual "toAst rejects new assignments in legacy code (2/2)."
                               (Left res :: Either [AstStmt] AbstractionErr)
                               (toAst legacy [cldecStmt, qldecStmt, lmeasStmt]))
    where res = [cdecAst, qdecAst, measure1Ast]

-----------------------------------------------------------------------------------------
-- Legacy void call checks.

quipfuncsCallInLegacyTest :: String -> Test.HUnit.Test
quipfuncsCallInLegacyTest name = TestCase (assertEqual msg expt act)
    where msg  = "toAst rejects " ++ name ++ " in legacy mode."
          expt = Right $ NonLegacyStmt 2 :: Either [AstStmt] AbstractionErr
          stmt = QasmExprStmt $ Call name [QasmId "qvar"]
          act  = toAst legacy [qldecStmt, stmt]

test27 = quipfuncsCallInLegacyTest "QInit0"
test28 = quipfuncsCallInLegacyTest "QInit1"
test29 = quipfuncsCallInLegacyTest "QTerm0"
test30 = quipfuncsCallInLegacyTest "QTerm1"
test31 = quipfuncsCallInLegacyTest "QDiscard"

test32 = TestCase (assertEqual "toAst rejects void measure in legacy code."
                               (Right err :: Either [AstStmt] AbstractionErr)
                               (toAst legacy [qldecStmt, measure2Stmt]))
    where err = NonLegacyStmt 2

test33 = TestCase (assertEqual "toAst accepts reset in legacy code."
                               (Left res :: Either [AstStmt] AbstractionErr)
                               (toAst legacy [qldecStmt, resetStmt]))
    where res = [qdecAst, resetAst]

quipfuncsCallInDefaultTest :: String -> Test.HUnit.Test
quipfuncsCallInDefaultTest name = TestCase (assertEqual msg expt act)
    where msg  = "toAst rejects " ++ name ++ " without quipfuncs.inc lib."
          expt = Right $ MissingLib 2 "quipfuncs.inc" :: Either [AstStmt] AbstractionErr
          stmt = QasmExprStmt $ Call name [QasmId "qvar"]
          act  = toAst stdhdr [qldecStmt, stmt]

test34 = quipfuncsCallInDefaultTest "QInit0"
test35 = quipfuncsCallInDefaultTest "QInit1"
test36 = quipfuncsCallInDefaultTest "QTerm0"
test37 = quipfuncsCallInDefaultTest "QTerm1"
test38 = quipfuncsCallInDefaultTest "QDiscard"

test39 = TestCase (assertEqual "toAst accepts void measure without quipfuncs.inc lib."
                               (Left res :: Either [AstStmt] AbstractionErr)
                               (toAst stdhdr [qldecStmt, measure2Stmt]))
    where res = [qdecAst, measure2Ast]

test40 = TestCase (assertEqual "toAst accepts reset without quipfuncs.inc lib."
                               (Left res :: Either [AstStmt] AbstractionErr)
                               (toAst stdhdr [qldecStmt, resetStmt]))
    where res = [qdecAst, resetAst]

-----------------------------------------------------------------------------------------
-- Legacy rvalue checks.

quipfuncsRValueInLegacyTest :: String -> Test.HUnit.Test
quipfuncsRValueInLegacyTest name = TestCase (assertEqual msg expt act)
    where msg  = "toAst rejects " ++ name ++ " in legacy mode."
          expt = Right $ NonLegacyStmt 3 :: Either [AstStmt] AbstractionErr
          stmt = QasmLAssignStmt (CVar "cvar") $ Call name []
          act  = toAst legacy [cldecStmt, qldecStmt, stmt]

test41 = quipfuncsRValueInLegacyTest "CInit0"
test42 = quipfuncsRValueInLegacyTest "CInit1"
test43 = quipfuncsRValueInLegacyTest "CTerm0"
test44 = quipfuncsRValueInLegacyTest "CTerm1"
test45 = quipfuncsRValueInLegacyTest "CDiscard"

test46 = TestCase (assertEqual "toAst rejects QMeas in legacy code."
                               (Right err :: Either [AstStmt] AbstractionErr)
                               (toAst legacy [cldecStmt, qldecStmt, stmt]))
    where stmt = QasmLAssignStmt (CVar "cvar") $ Call "QMeas" [QasmId "qvar"]
          err  = NonLegacyStmt 3

test47 = TestCase (assertEqual "toAst accepts measure assignment in legacy mode."
                               (Left res :: Either [AstStmt] AbstractionErr)
                               (toAst legacy [cldecStmt, qldecStmt, lmeasStmt]))
    where res = [cdecAst, qdecAst, measure1Ast]

quipfuncsRValueInDefaultTest :: String -> Test.HUnit.Test
quipfuncsRValueInDefaultTest name = TestCase (assertEqual msg expt act)
    where msg  = "toAst rejects " ++ name ++ " without quipfuncs.inc lib."
          expt = Right $ MissingLib 3 "quipfuncs.inc" :: Either [AstStmt] AbstractionErr
          stmt = QasmAssignStmt (CVar "cvar") $ Call name []
          act  = toAst stdhdr [cldecStmt, qldecStmt, stmt]

test48 = quipfuncsRValueInDefaultTest "CInit0"
test49 = quipfuncsRValueInDefaultTest "CInit1"
test50 = quipfuncsRValueInDefaultTest "CTerm0"
test51 = quipfuncsRValueInDefaultTest "CTerm1"
test52 = quipfuncsRValueInDefaultTest "CDiscard"

test53 = TestCase (assertEqual "toAst rejects QMeas without quipfuncs.inc lib."
                               (Right err :: Either [AstStmt] AbstractionErr)
                               (toAst stdhdr [cldecStmt, qldecStmt, stmt]))
    where stmt = QasmAssignStmt (CVar "cvar") $ Call "QMeas" [QasmId "qvar"]
          err  = MissingLib 3 "quipfuncs.inc"

test54 = TestCase (assertEqual "toAst accepts measure assignment in legacy mode."
                               (Left res :: Either [AstStmt] AbstractionErr)
                               (toAst stdhdr [cldecStmt, qldecStmt, lmeasStmt]))
    where res = [cdecAst, qdecAst, measure1Ast]

-----------------------------------------------------------------------------------------
-- Gate import checks.

missingQe1libTest :: String -> Test.HUnit.Test
missingQe1libTest name = TestCase (assertEqual msg expt act)
    where msg  = "toAst rejects gate " ++ name ++ " without qelib1.inc included."
          expt = Right $ MissingLib 2 "qelib1.inc"
          gate = QasmGateStmt $ NamedGateOp name [] [QVar "qvar"]
          act  = toAst defLegacy [qldecStmt, gate]

test55 = missingQe1libTest "x"
test56 = missingQe1libTest "y"
test57 = missingQe1libTest "z"

missingBkpgatesTest :: String -> [Expr] -> Test.HUnit.Test
missingBkpgatesTest name args = TestCase (assertEqual msg expt act)
    where msg  = "toAst rejects gate " ++ name ++ " without bkpgates.inc included."
          expt = Right $ MissingLib 2 "bkpgates.inc"
          gate = QasmGateStmt $ NamedGateOp name args [QVar "qvar"]
          act  = toAst defLegacy [qldecStmt, gate]

test58 = missingBkpgatesTest "sx" []
test59 = missingBkpgatesTest "p" [Pi]

missingStdgatesTest :: String -> [Expr] -> Test.HUnit.Test
missingStdgatesTest name args = TestCase (assertEqual msg expt act)
    where msg  = "toAst rejects gate " ++ name ++ " without stdgates.inc included."
          expt = Right $ MissingLib 2 "stdgates.inc"
          gate = QasmGateStmt $ NamedGateOp name args [QVar "qvar"]
          act  = toAst defhdr [qldecStmt, gate]

test60 = missingStdgatesTest "x" []
test61 = missingStdgatesTest "y" []
test62 = missingStdgatesTest "z" []
test63 = missingStdgatesTest "sx" []
test64 = missingStdgatesTest "p" [Pi]

missingQuipgatesIn2Test :: String -> Test.HUnit.Test
missingQuipgatesIn2Test name = TestCase (assertEqual msg expt act)
    where msg  = "toAst rejects gate " ++ name ++ " without quipgates.inc in version 2."
          expt = Right $ MissingLib 2 "quipgates.inc"
          gate = QasmGateStmt $ NamedGateOp name [] [QVar "qvar"]
          act  = toAst defLegacy [qldecStmt, gate]

test65 = missingQuipgatesIn2Test "quip_ix"
test66 = missingQuipgatesIn2Test "quip_e"
test67 = missingQuipgatesIn2Test "quip_omega"

missingQuipgatesIn3Test :: String -> Test.HUnit.Test
missingQuipgatesIn3Test name = TestCase (assertEqual msg expt act)
    where msg  = "toAst rejects gate " ++ name ++ " without quipgates.inc in version 2."
          expt = Right $ MissingLib 2 "quipgates.inc"
          gate = QasmGateStmt $ NamedGateOp name [] [QVar "qvar"]
          act  = toAst defhdr [qldecStmt, gate]

test68 = missingQuipgatesIn3Test "quip_ix"
test69 = missingQuipgatesIn3Test "quip_e"
test70 = missingQuipgatesIn3Test "quip_omega"

allIncludesIn2Test :: String -> [Expr] -> Test.HUnit.Test
allIncludesIn2Test name args = TestCase (assertBool msg res)
    where msg  = "toAst accepts gate " ++ name ++ " in version 2.0 with all includes."
          gate = QasmGateStmt $ NamedGateOp name args [QVar "qvar"]
          res  = isLeft $ toAst legacyAll [qldecStmt, gate]

test71 = allIncludesIn2Test "x" []
test72 = allIncludesIn2Test "y" []
test73 = allIncludesIn2Test "z" []
test74 = allIncludesIn2Test "sx" []
test75 = allIncludesIn2Test "p" [Pi]
test76 = allIncludesIn2Test "quip_ix" []
test77 = allIncludesIn2Test "quip_e" []
test78 = allIncludesIn2Test "quip_omega" []

allIncludesIn3Test :: String -> [Expr] -> Test.HUnit.Test
allIncludesIn3Test name args = TestCase (assertBool msg res)
    where msg  = "toAst accepts gate " ++ name ++ " in version 3 with all includes."
          gate = QasmGateStmt $ NamedGateOp name args [QVar "qvar"]
          res  = isLeft $ toAst libhdr [qldecStmt, gate]

test79 = allIncludesIn3Test "x" []
test80 = allIncludesIn3Test "y" []
test81 = allIncludesIn3Test "z" []
test82 = allIncludesIn3Test "sx" []
test83 = allIncludesIn3Test "p" [Pi]
test84 = allIncludesIn3Test "quip_ix" []
test85 = allIncludesIn3Test "quip_e" []
test86 = allIncludesIn3Test "quip_omega" []

-----------------------------------------------------------------------------------------
-- elimFun.

test87 = TestCase (assertEqual "elimFun supports empty files."
                               (Left [] :: Either [AstStmt] InlineErr)
                               (elimFun []))

test88 = TestCase (assertEqual "elimFun for void calls."
                               (Left elims :: Either [AstStmt] InlineErr)
                               (elimFun input))
    where decl1 = AstQubitDecl Nothing "q1"
          decl2 = AstQubitDecl (Just 5) "q2"
          call1 = AstCall $ QuipQInit0 $ QRef "q1"
          call2 = AstCall $ QuipQInit1 $ Cell "q2" 2
          call3 = AstCall $ QuipQTerm0 $ QRef "q1"
          call4 = AstCall $ QuipQTerm1 $ Cell "q2" 2
          call5 = AstCall $ QuipQDiscard $ Cell "q2" 4
          call6 = AstCall $ VoidMeasure $ Cell "q2" 0
          call7 = AstCall $ VoidReset $ Cell "q2" 0
          decl3 = AstBitDecl Nothing "q3"
          input = [decl1, decl2, call1, call2, call3, call4, call5, call6, call7, decl3]
          out1a = AstCall $ VoidReset $ QRef "q1"
          out2a = AstCall $ VoidReset $ Cell "q2" 2
          out2b = AstGateStmt 0 $ NamedGate GateX [] [Cell "q2" 2] nullGateMod
          elims = [decl1, decl2, out1a, out2a, out2b, call6, call7, decl3]

test89 = TestCase (assertEqual "elimFun for assignment (no classical initialization)."
                               (Left elims :: Either [AstStmt] InlineErr)
                               (elimFun input))
    where decl1 = AstQubitDecl Nothing "q1"
          decl2 = AstQubitDecl (Just 5) "q2"
          decl3 = AstBitDecl Nothing "c1"
          asgn1 = AstAssign "c1" Nothing $ QuipMeasure $ QRef "q1"
          asgn2 = AstAssign "c1" Nothing $ Measure $ Cell "q2" 3
          decl4 = AstBitDecl (Just 5) "c2"
          asgn3 = AstAssign "c2" (Just 1) QuipCTerm0
          asgn4 = AstAssign "c2" (Just 3) QuipCTerm1
          asgn5 = AstAssign "c2" (Just 0) QuipCDiscard
          gate1 = AstGateStmt 0 $ NamedGate GateX [] [Cell "q2" 2] nullGateMod
          input = [decl1, decl2, decl3, asgn1, asgn2, decl4, asgn3, asgn4, asgn5, gate1]
          out1a = AstAssign "c1" Nothing $ Measure $ QRef "q1"
          elims = [decl1, decl2, decl3, out1a, asgn2, decl4, gate1]

mk_gate_1 :: Expr -> AstStmt
mk_gate_1 arg = AstGateStmt 1 $ NamedGate GateCY [arg] [Cell "q1" 3] nullGateMod

mk_gate_2 :: Expr -> AstStmt
mk_gate_2 arg = AstGateStmt 2 $ NamedGate GateU2 args [Cell "q1" 3, Cell "q1" 0] mods
    where mods = addCtrlsToMod 1 $ negateMod nullGateMod
          args = [arg, Call "cos" [Div Pi $ DecInt "2"]]

eval_as_float :: Expr -> Expr
eval_as_float = DecFloat . show . fromLeft 0 . toConstFloat

test90 = TestCase (assertEqual "elimFun for gates."
                               (Left elims :: Either [AstStmt] InlineErr)
                               (elimFun input))
    where expr1 = Call "arcsin" [DecInt "1"]
          expr2 = Call "arccos" [DecFloat "0.5"]
          mods1 = addCtrlsToMod 1 nullGateMod
          mods2 = negateMod nullGateMod
          decls = AstQubitDecl (Just 5) "q1"
          gate1 = AstGateStmt 0 $ NamedGate GateP [Pi] [Cell "q1" 1, Cell "q1" 0] mods1
          gate2 = mk_gate_1 expr1
          gate3 = mk_gate_2 $ Plus Euler $ expr2
          gate4 = AstGateStmt 3 $ NamedGate GateCX [] [Cell "q1" 1, Cell "q1" 2] mods2
          input = [decls, gate1, gate2, gate3, gate4]
          out2a = mk_gate_1 $ eval_as_float expr1
          out3a = mk_gate_2 $ Plus Euler $ eval_as_float expr2
          elims = [decls, gate1, out2a, out3a, gate4]

test91 = TestCase (assertEqual "elimFun handles inline failures for gata parameters."
                               (Right errs :: Either [AstStmt] InlineErr)
                               (elimFun [decl, gate]))
    where mods = nullGateMod
          decl = AstQubitDecl (Just 5) "q1"
          gate = AstGateStmt 0 $ NamedGate GateP [Call "f" []] [Cell "q1" 1] mods
          errs = FailedToEval 2 "f"

-----------------------------------------------------------------------------------------
-- toLsc.

test92 = TestCase (assertEqual "toLsc handles valid translations."
                               (Left louts :: Either [AstStmt] ToLscErr)
                               (toLsc input))
    where decl1 = AstQubitDecl Nothing "q1"
          gate1 = AstGateStmt 0 $ NamedGate GateZ [] [QRef "q1"] nullGateMod
          gate2 = AstGateStmt 0 $ NamedGate GateID [] [QRef "q1"] nullGateMod
          gate3 = AstGateStmt 0 $ NamedGate GateQuipIX [] [QRef "q1"] nullGateMod
          decl2 = AstBitDecl Nothing "c1"
          asgn1 = AstAssign "c1" Nothing $ QuipMeasure $ QRef "q1"
          call1 = AstCall $ VoidReset $ QRef "q1"
          input = [decl1, gate1, gate2, gate3, decl2, asgn1, call1]
          out3a = AstGateStmt 0 $ NamedGate GateX [] [QRef "q1"] nullGateMod
          louts = [decl1, gate1, out3a, decl2, asgn1, call1]

test93 = TestCase (assertEqual "toLsc handles rewriteLscGate errors."
                               (Right errs :: Either [AstStmt] ToLscErr)
                               (toLsc [decl, gate]))
    where decl = AstQubitDecl Nothing "q"
          gate = AstGateStmt 0 $ NamedGate GateU [] [QRef "q"] nullGateMod
          errs = LscRewriteFailure 2 $ UnsupportedCompilation GateU

test94 = TestCase (assertEqual "toLsc handles power modifier errors."
                               (Right errs :: Either [AstStmt] ToLscErr)
                               (toLsc [decl, gate]))
    where decl = AstQubitDecl Nothing "q"
          gate = AstGateStmt 5 $ NamedGate GateX [] [QRef "q"] nullGateMod
          errs = UnexpectedPowerMod 2

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
                                     TestLabel "toAst_BadMeasureExpr" test22,
                                     TestLabel "toAst_LegacyDecl_1" test23,
                                     TestLabel "toAst_LegacyDecl_2" test24,
                                     TestLabel "toAst_LegacyAssign_1" test25,
                                     TestLabel "toAst_LegacyAssign_2" test26,
                                     TestLabel "toAst_LegacyCall_1" test27,
                                     TestLabel "toAst_LegacyCall_2" test28,
                                     TestLabel "toAst_LegacyCall_3" test29,
                                     TestLabel "toAst_LegacyCall_4" test30,
                                     TestLabel "toAst_LegacyCall_5" test31,
                                     TestLabel "toAst_LegacyCall_6" test32,
                                     TestLabel "toAst_LegacyCall_7" test33,
                                     TestLabel "toAst_StdCall_1" test34,
                                     TestLabel "toAst_StdCall_2" test35,
                                     TestLabel "toAst_StdCall_3" test36,
                                     TestLabel "toAst_StdCall_4" test37,
                                     TestLabel "toAst_StdCall_5" test38,
                                     TestLabel "toAst_StdCall_6" test39,
                                     TestLabel "toAst_StdCall_7" test40,
                                     TestLabel "toAst_LegacyRV_1" test41,
                                     TestLabel "toAst_LegacyRV_2" test42,
                                     TestLabel "toAst_LegacyRV_3" test43,
                                     TestLabel "toAst_LegacyRV_4" test44,
                                     TestLabel "toAst_LegacyRV_5" test45,
                                     TestLabel "toAst_LegacyRV_6" test46,
                                     TestLabel "toAst_LegacyRV_7" test47,
                                     TestLabel "toAst_StdRV_1" test48,
                                     TestLabel "toAst_StdRV_2" test49,
                                     TestLabel "toAst_StdRV_3" test50,
                                     TestLabel "toAst_StdRV_4" test51,
                                     TestLabel "toAst_StdRV_5" test52,
                                     TestLabel "toAst_StdRV_6" test53,
                                     TestLabel "toAst_StdRV_7" test54,
                                     TestLabel "toAst_MissingQelib1_x" test55,
                                     TestLabel "toAst_MissingQelib1_y" test56,
                                     TestLabel "toAst_MissingQelib1_z" test57,
                                     TestLabel "toAst_MissingBkpgates_sx" test58,
                                     TestLabel "toAst_MissingBkpgates_p" test59,
                                     TestLabel "toAst_MissingStdgates_x" test60,
                                     TestLabel "toAst_MissingStdgates_y" test61,
                                     TestLabel "toAst_MissingStdgates_z" test62,
                                     TestLabel "toAst_MissingStdgates_sx" test63,
                                     TestLabel "toAst_MissingStdgates_p" test64,
                                     TestLabel "toAst_MissingQuipgates2_ix" test65,
                                     TestLabel "toAst_MissingQuipgates2_e" test66,
                                     TestLabel "toAst_MissingQuipgates2_omega" test67,
                                     TestLabel "toAst_MissingQuipgates3_ix" test68,
                                     TestLabel "toAst_MissingQuipgates3_e" test69,
                                     TestLabel "toAst_MissingQuipgates3_omega" test70,
                                     TestLabel "toAst_ValidIn2_x" test71,
                                     TestLabel "toAst_ValidIn2_y" test72,
                                     TestLabel "toAst_ValidIn2_z" test73,
                                     TestLabel "toAst_ValidIn2_sx" test74,
                                     TestLabel "toAst_ValidIn2_p" test75,
                                     TestLabel "toAst_ValidIn2_ix" test76,
                                     TestLabel "toAst_ValidIn2_e" test77,
                                     TestLabel "toAst_ValidIn2_omega" test78,
                                     TestLabel "toAst_ValidIn3_x" test79,
                                     TestLabel "toAst_ValidIn3_y" test80,
                                     TestLabel "toAst_ValidIn3_z" test81,
                                     TestLabel "toAst_ValidIn3_sx" test82,
                                     TestLabel "toAst_ValidIn3_p" test83,
                                     TestLabel "toAst_ValidIn3_ix" test84,
                                     TestLabel "toAst_ValidIn3_e" test85,
                                     TestLabel "toAst_ValidIn3_omega" test86,
                                     TestLabel "elimFun_EmptyFile" test87,
                                     TestLabel "elimFun_VoidCall" test88,
                                     TestLabel "elimFun_Assign" test89,
                                     TestLabel "elimFun_Gate" test90,
                                     TestLabel "elimFun_GateFail" test91,
                                     TestLabel "toLsc_ValidInput" test92,
                                     TestLabel "toLsc_Error_Lsc" test93,
                                     TestLabel "toLsc_Error_Pow" test94]

main = defaultMain tests
