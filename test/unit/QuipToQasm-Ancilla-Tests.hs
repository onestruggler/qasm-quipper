module Main where

import qualified Data.IntMap.Strict as IntMap
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.AST
import LinguaQuanta.Qasm.Operand
import LinguaQuanta.Quip.Wire
import LinguaQuanta.QuipToQasm.Ancilla
import LinguaQuanta.QuipToQasm.Wire

-----------------------------------------------------------------------------------------
-- Wires and allocations.

inputs = [(1, QWire), (5, QWire), (3, CWire), (44, CWire)]
allocs = allocateInputWires $ IntMap.fromList inputs

-----------------------------------------------------------------------------------------
-- translateQInit and translateQTerm (testing translateQInit requires translateQTerm).

(allocs1, stmts1) = translateQInit allocs 72 False
(allocs2, stmts2) = translateQTerm allocs1 72 False
(allocs3, stmts3) = translateQInit allocs2 72 True
(allocs4, stmts4) = translateQTerm allocs3 72 True

test1 = TestCase (assertEqual "translateQInit for QInit0 with new declaration (stmts)."
                              [AstQubitDecl Nothing "shadow_qwire_0",
                               AstCall $ QuipQInit0 $ QRef "shadow_qwire_0"]
                              stmts1)

test2 = TestCase (assertEqual "translateQInit for QInit0 with new declaration (state)."
                              (Just QWire :: Maybe WireType)
                              (getState 72 allocs1))

test3 = TestCase (assertEqual "translateQTerm for QTerm0 (stmts)."
                              [AstCall $ QuipQTerm0 $ QRef "shadow_qwire_0"]
                              stmts2)

test4 = TestCase (assertEqual "translateQTerm for QTerm0 (state)."
                              (Nothing :: Maybe WireType)
                              (getState 72 allocs2))

test5 = TestCase (assertEqual "translateQInit for QInit1 with old declaration (stmts)."
                              [AstCall $ QuipQInit1 $ QRef "shadow_qwire_0"]
                              stmts3)

test6 = TestCase (assertEqual "translateQInit for QInit1 with old declaration (state)."
                              (Just QWire :: Maybe WireType)
                              (getState 72 allocs3))

test7 = TestCase (assertEqual "translateQTerm for QTerm1 (stmts)."
                              [AstCall $ QuipQTerm1 $ QRef "shadow_qwire_0"]
                              stmts4)

test8 = TestCase (assertEqual "translateQTerm for QTerm1 (state)."
                              (Nothing :: Maybe WireType)
                              (getState 72 allocs4))

test9 = TestCase (assertEqual "translate{QInit,QTerm} leave wires untouched. (1/4)"
                              (Just QWire :: Maybe WireType)
                              (getState 1 allocs4))

test10 = TestCase (assertEqual "translate{QInit,QTerm} leave wires untouched. (2/4)"
                               (Just QWire :: Maybe WireType)
                               (getState 5 allocs4))

test11 = TestCase (assertEqual "translate{QInit,QTerm} leave wires untouched. (3/4)"
                               (Just CWire :: Maybe WireType)
                               (getState 3 allocs4))

test12 = TestCase (assertEqual "translate{QInit,QTerm} leave wires untouched. (4/4)"
                               (Just CWire :: Maybe WireType)
                               (getState 44 allocs4))

-----------------------------------------------------------------------------------------
-- translateQDiscard

(allocs5, stmts5) = translateQDiscard allocs 1

test13 = TestCase (assertEqual "translateQDiscard (stmts)."
                               [AstCall $ QuipQDiscard $ Cell "input_qwires" 0]
                               stmts5)

test14 = TestCase (assertEqual "translateQDiscard (state)."
                               (Nothing :: Maybe WireType)
                               (getState 1 allocs5))

test15 = TestCase (assertEqual "translateQDiscard leave wires untouched. (1/3)"
                               (Just QWire :: Maybe WireType)
                               (getState 5 allocs5))

test16 = TestCase (assertEqual "translateQDiscard leave wires untouched. (2/3)"
                               (Just CWire :: Maybe WireType)
                               (getState 3 allocs5))

test17 = TestCase (assertEqual "translateQDiscard leave wires untouched. (3/3)"
                               (Just CWire :: Maybe WireType)
                               (getState 44 allocs5))

-----------------------------------------------------------------------------------------
-- translateCInit and translateCTerm (testing translateCInit requires translateCTerm).

(allocs6, stmts6) = translateCInit allocs 72 False
(allocs7, stmts7) = translateCTerm allocs6 72 False
(allocs8, stmts8) = translateCInit allocs7 72 True
(allocs9, stmts9) = translateCTerm allocs8 72 True

test18 = TestCase (assertEqual "translateQInit for CInit0 with new declaration (stmts)."
                               [AstBitDecl Nothing "shadow_cwire_0",
                                AstAssign "shadow_cwire_0" Nothing QuipCInit0]
                               stmts6)

test19 = TestCase (assertEqual "translateCInit for CInit0 with new declaration (state)."
                               (Just CWire :: Maybe WireType)
                               (getState 72 allocs6))

test20 = TestCase (assertEqual "translateCTerm for CTerm0 (stmts)."
                               [AstAssign "shadow_cwire_0" Nothing QuipCTerm0]
                               stmts7)

test21 = TestCase (assertEqual "translateCTerm for CTerm0 (state)."
                               (Nothing :: Maybe WireType)
                               (getState 72 allocs7))

test22 = TestCase (assertEqual "translateCInit for CInit1 with old declaration (stmts)."
                               [AstAssign "shadow_cwire_0" Nothing QuipCInit1]
                               stmts8)

test23 = TestCase (assertEqual "translateCInit for CInit1 with old declaration (state)."
                               (Just CWire :: Maybe WireType)
                               (getState 72 allocs8))

test24 = TestCase (assertEqual "translateCTerm for CTerm1 (stmts)."
                               [AstAssign "shadow_cwire_0" Nothing QuipCTerm1]
                               stmts9)

test25 = TestCase (assertEqual "translateQTerm for CTerm1 (state)."
                               (Nothing :: Maybe WireType)
                               (getState 72 allocs9))

test26 = TestCase (assertEqual "translate{CInit,CTerm} leave wires untouched. (1/4)"
                               (Just QWire :: Maybe WireType)
                               (getState 1 allocs9))

test27 = TestCase (assertEqual "translate{CInit,CTerm} leave wires untouched. (2/4)"
                               (Just QWire :: Maybe WireType)
                               (getState 5 allocs9))

test28 = TestCase (assertEqual "translate{CInit,CTerm} leave wires untouched. (3/4)"
                               (Just CWire :: Maybe WireType)
                               (getState 3 allocs9))

test29 = TestCase (assertEqual "translate{CInit,CTerm} leave wires untouched. (4/4)"
                               (Just CWire :: Maybe WireType)
                               (getState 44 allocs9))

-----------------------------------------------------------------------------------------
-- translateCDiscard

(allocs10, stmts10) = translateCDiscard allocs 44

test30 = TestCase (assertEqual "translateCDiscard (stmts)."
                               [AstAssign "input_cwires" (Just 1) QuipCDiscard]
                               stmts10)

test31 = TestCase (assertEqual "translateCDiscard (state)."
                               (Nothing :: Maybe WireType)
                               (getState 44 allocs10))

test32 = TestCase (assertEqual "translateCDiscard leave wires untouched. (1/3)"
                               (Just QWire :: Maybe WireType)
                               (getState 1 allocs10))

test33 = TestCase (assertEqual "translateCDiscard leave wires untouched. (2/3)"
                               (Just QWire :: Maybe WireType)
                               (getState 5 allocs10))

test34 = TestCase (assertEqual "translateCDiscard leave wires untouched. (3/3)"
                               (Just CWire :: Maybe WireType)
                               (getState 3 allocs10))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "QInit0_NewDecl_Stmts" test1,
                                     TestLabel "QInit0_NewDecl_State" test2,
                                     TestLabel "QTerm0_Stmts" test3,
                                     TestLabel "QTerm0_State" test4,
                                     TestLabel "QInit1_ReuseDecl_Stmts" test5,
                                     TestLabel "QInit1_ReuseDecl_State" test6,
                                     TestLabel "QTerm1_Stmts" test7,
                                     TestLabel "QTerm1_State" test8,
                                     TestLabel "QInitQTerm_Untouched_1" test9,
                                     TestLabel "QInitQTerm_Untouched_2" test10,
                                     TestLabel "QInitQTerm_Untouched_3" test11,
                                     TestLabel "QInitQTerm_Untouched_4" test12,
                                     TestLabel "QDiscard_Stmts" test13,
                                     TestLabel "QDiscard_State" test14,
                                     TestLabel "QDiscard_Untouched_1" test15,
                                     TestLabel "QDiscard_Untouched_2" test16,
                                     TestLabel "QDiscard_Untouched_3" test17,
                                     TestLabel "CInit0_NewDecl_Stmts" test18,
                                     TestLabel "CInit0_NewDecl_State" test19,
                                     TestLabel "CTerm0_Stmts" test20,
                                     TestLabel "CTerm0_State" test21,
                                     TestLabel "CInit1_ReuseDecl_Stmts" test22,
                                     TestLabel "CInit1_ReuseDecl_State" test23,
                                     TestLabel "CTerm1_Stmts" test24,
                                     TestLabel "CTerm1_State" test25,
                                     TestLabel "CInitQTerm_Untouched_1" test26,
                                     TestLabel "CInitQTerm_Untouched_2" test27,
                                     TestLabel "CInitQTerm_Untouched_3" test28,
                                     TestLabel "CInitQTerm_Untouched_4" test29,
                                     TestLabel "CDiscard_Stmts" test30,
                                     TestLabel "CDiscard_State" test31,
                                     TestLabel "CDiscard_Untouched_1" test32,
                                     TestLabel "CDiscard_Untouched_2" test33,
                                     TestLabel "CDiscard_Untouched_3" test34]

main = defaultMain tests
