module Main where

import qualified Data.IntMap.Strict as IntMap
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.Operand
import LinguaQuanta.QasmToQuip.Assign
import LinguaQuanta.QasmToQuip.Wire
import LinguaQuanta.Quip.Gate
import LinguaQuanta.Quip.GateName
import LinguaQuanta.Quip.Wire

-----------------------------------------------------------------------------------------
-- Sets up a simple wire allocation map.

(Just allocsTmp0) = allocate QWire "var1" Nothing initialAllocations
(Just allocsTmp1) = allocate CWire "var2" Nothing allocsTmp0
(Just allocsTmp2) = allocate QWire "var3" (Just 3) allocsTmp1
(Just allocs)     = allocate QWire "var4" (Just 5) allocsTmp2

-----------------------------------------------------------------------------------------
-- translateCDiscard

(discardMap1, discardGates1) = translateCDiscard allocs "var1" Nothing
(discardMap2, discardGates2) = translateCDiscard allocs "var3" $ Just 2

test1 = TestCase (assertEqual "translateCDiscard: with QRef operand (map)."
                              9
                              (IntMap.size $ toQuipperOutputs discardMap1))

test2 = TestCase (assertEqual "translateCDiscard: with QRef operand (gates)."
                              [CDiscardGate 0]
                              discardGates1)

test3 = TestCase (assertEqual "translateCDiscard: with Cell operand (map)."
                              9
                              (IntMap.size $ toQuipperOutputs discardMap2))

test4 = TestCase (assertEqual "translateCDiscard: with Cell operand (gates)."
                              [CDiscardGate 4]
                              discardGates2)

-----------------------------------------------------------------------------------------
-- translateCInit0

(initZeroMap1, initZeroGates1) = translateCInit0 allocs "var1" Nothing
(initZeroMap2, initZeroGates2) = translateCInit0 allocs "var3" $ Just 2

test5 = TestCase (assertEqual "translateCInit0: with QRef operand (map)."
                              9
                              (IntMap.size $ toQuipperInputs initZeroMap1))

test6 = TestCase (assertEqual "translateCInit0: with QRef operand (gates)."
                              [CInitGate False 0]
                              initZeroGates1)

test7 = TestCase (assertEqual "translateCInit0: with Cell operand (map)."
                              9
                              (IntMap.size $ toQuipperInputs initZeroMap2))

test8 = TestCase (assertEqual "translateCInit0: with Cell operand (gates)."
                              [CInitGate False 4]
                              initZeroGates2)

-----------------------------------------------------------------------------------------
-- translateCInit1

(initOneMap1, initOneGates1) = translateCInit1 allocs "var1" Nothing
(initOneMap2, initOneGates2) = translateCInit1 allocs "var3" $ Just 2

test9 = TestCase (assertEqual "translateCInit1: with QRef operand (map)."
                              9
                              (IntMap.size $ toQuipperInputs initOneMap1))

test10 = TestCase (assertEqual "translateCInit1: with QRef operand (gates)."
                               [CInitGate True 0]
                               initOneGates1)

test11 = TestCase (assertEqual "translateCInit1: with Cell operand (map)."
                               9
                               (IntMap.size $ toQuipperInputs initOneMap2))

test12 = TestCase (assertEqual "translateCInit1: with Cell operand (gates)."
                               [CInitGate True 4]
                               initOneGates2)

-----------------------------------------------------------------------------------------
-- translateCTerm0

(termZeroMap1, termZeroGates1) = translateCTerm0 allocs "var1" Nothing
(termZeroMap2, termZeroGates2) = translateCTerm0 allocs "var3" $ Just 2

test13 = TestCase (assertEqual "translateCTerm0: with QRef operand (map)."
                               9
                               (IntMap.size $ toQuipperOutputs termZeroMap1))

test14 = TestCase (assertEqual "translateCTerm0: with QRef operand (gates)."
                               [CTermGate False 0]
                               termZeroGates1)

test15 = TestCase (assertEqual "translateCTerm0: with Cell operand (map)."
                               9
                               (IntMap.size $ toQuipperOutputs termZeroMap2))

test16 = TestCase (assertEqual "translateCTerm0: with Cell operand (gates)."
                               [CTermGate False 4]
                               termZeroGates2)

-----------------------------------------------------------------------------------------
-- translateCTerm1

(termOneMap1, termOneGates1) = translateCTerm1 allocs "var1" Nothing
(termOneMap2, termOneGates2) = translateCTerm1 allocs "var3" $ Just 2

test17 = TestCase (assertEqual "translateCTerm1: with QRef operand (map)."
                               9
                               (IntMap.size $ toQuipperOutputs termOneMap1))

test18 = TestCase (assertEqual "translateCTerm1: with QRef operand (gates)."
                               [CTermGate True 0]
                               termOneGates1)

test19 = TestCase (assertEqual "translateCTerm1: with Cell operand (map)."
                               9
                               (IntMap.size $ toQuipperOutputs termOneMap2))

test20 = TestCase (assertEqual "translateCTerm1: with Cell operand (gates)."
                               [CTermGate True 4]
                               termOneGates2)

-----------------------------------------------------------------------------------------
-- * Measurement Translation.

(Just allocsTmp3) = allocate QWire "qvar" Nothing initialAllocations
(Just allocsTmp4) = allocate CWire "cvar" Nothing allocsTmp3
(Just allocsTmp5) = allocate QWire "qreg" (Just 4) allocsTmp4
(Just measAllocs) = allocate CWire "creg" (Just 5) allocsTmp5

qvar = QRef "qvar"
qreg = Cell "qreg" 2

test21 = TestCase (assertEqual "translateMeasure: QVar to CVar."
                               [CDiscardGate 1,
                                QInitGate False 1,
                                NamedGate GateX False [1] [Pos 0],
                                QMeasGate 1]
                               (translateMeasure measAllocs "cvar" Nothing qvar))

test22 = TestCase (assertEqual "translateMeasure: QVar to CReg."
                               [CDiscardGate 7,
                                QInitGate False 7,
                                NamedGate GateX False [7] [Pos 0],
                                QMeasGate 7]
                               (translateMeasure measAllocs "creg" (Just 1) qvar))

test23 = TestCase (assertEqual "translateMeasure: QReg to CVar."
                               [CDiscardGate 1,
                                QInitGate False 1,
                                NamedGate GateX False [1] [Pos 4],
                                QMeasGate 1]
                               (translateMeasure measAllocs "cvar" Nothing qreg))

test24 = TestCase (assertEqual "translateMeasure: QReg to CReg."
                               [CDiscardGate 7,
                                QInitGate False 7,
                                NamedGate GateX False [7] [Pos 4],
                                QMeasGate 7]
                               (translateMeasure measAllocs "creg" (Just 1) qreg))

-----------------------------------------------------------------------------------------
-- * QMeas Translation.

(qmeasMap1, qmeasGates1) = translateQMeas measAllocs "cvar" Nothing qvar

test25 = TestCase (assertEqual "translateQMeas: QVar to CVar (gates)."
                               [QMeasGate 0]
                               qmeasGates1)

test26 = TestCase (assertEqual "translateQMeas: QVar to CVar (state, 1/2)."
                               (Just CWire :: Maybe WireType)
                               (IntMap.lookup 0 $ toQuipperOutputs qmeasMap1))

test27 = TestCase (assertEqual "translateQMeas: QVar to CVar (state, 2/2)."
                               (Nothing :: Maybe WireType)
                               (IntMap.lookup 1 $ toQuipperOutputs qmeasMap1))

(qmeasMap2, qmeasGates2) = translateQMeas measAllocs "creg" (Just 1) qvar

test28 = TestCase (assertEqual "translateQMeas: QVar to CReg (gates)."
                               [QMeasGate 0]
                               qmeasGates2)

test29 = TestCase (assertEqual "translateQMeas: QVar to CReg (state, 1/2)."
                               (Just CWire :: Maybe WireType)
                               (IntMap.lookup 0 $ toQuipperOutputs qmeasMap2))

test30 = TestCase (assertEqual "translateQMeas: QVar to CReg (state, 2/2)."
                               (Nothing :: Maybe WireType)
                               (IntMap.lookup 7 $ toQuipperOutputs qmeasMap2))

(qmeasMap3, qmeasGates3) = translateQMeas measAllocs "cvar" Nothing qreg

test31 = TestCase (assertEqual "translateQMeas: QReg to CVar (gates)."
                               [QMeasGate 4]
                               qmeasGates3)

test32 = TestCase (assertEqual "translateQMeas: QReg to CVar (state, 1/2)."
                               (Just CWire :: Maybe WireType)
                               (IntMap.lookup 4 $ toQuipperOutputs qmeasMap3))

test33 = TestCase (assertEqual "translateQMeas: QReg to CVar (state, 2/2)."
                               (Nothing :: Maybe WireType)
                               (IntMap.lookup 1 $ toQuipperOutputs qmeasMap3))

(qmeasMap4, qmeasGates4) = translateQMeas measAllocs "creg" (Just 1) qreg

test34 = TestCase (assertEqual "translateQMeas: QReg to CReg (gates)."
                               [QMeasGate 4]
                               qmeasGates4)

test35 = TestCase (assertEqual "translateQMeas: QReg to CReg (state, 1/2)."
                               (Just CWire :: Maybe WireType)
                               (IntMap.lookup 4 $ toQuipperOutputs qmeasMap4))

test36 = TestCase (assertEqual "translateQMeas: QReg to CReg (state, 2/2)."
                               (Nothing :: Maybe WireType)
                               (IntMap.lookup 7 $ toQuipperOutputs qmeasMap4))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "translateCDiscard_QRef_Map" test1,
                                     TestLabel "translateCDiscard_QRef_Gates" test2,
                                     TestLabel "translateCDiscard_Cell_Map" test3,
                                     TestLabel "translateCDiscard_Cell_Gates" test4,
                                     TestLabel "translateCInit0_QRef_Map" test5,
                                     TestLabel "translateCInit0_QRef_Gates" test6,
                                     TestLabel "translateCInit0_Cell_Map" test7,
                                     TestLabel "translateCInit0_Cell_Gates" test8,
                                     TestLabel "translateCInit1_QRef_Map" test9,
                                     TestLabel "translateCInit1_QRef_Gates" test10,
                                     TestLabel "translateCInit1_Cell_Map" test11,
                                     TestLabel "translateCInit1_Cell_Gates" test12,
                                     TestLabel "translateCTerm0_QRef_Map" test13,
                                     TestLabel "translateCTerm0_QRef_Gates" test14,
                                     TestLabel "translateCTerm0_Cell_Map" test15,
                                     TestLabel "translateCTerm0_Cell_Gates" test16,
                                     TestLabel "translateCTerm1_QRef_Map" test17,
                                     TestLabel "translateCTerm1_QRef_Gates" test18,
                                     TestLabel "translateCTerm1_Cell_Map" test19,
                                     TestLabel "translateCTerm1_Cell_Gates" test20,
                                     TestLabel "translateMeasure_QVar_CVar" test21,
                                     TestLabel "translateMeasure_QVar_CReg" test22,
                                     TestLabel "translateMeasure_QReg_CVar" test23,
                                     TestLabel "translateMeasure_QReg_CReg" test24,
                                     TestLabel "translateQMeas_QVar_CVar_Gates" test25,
                                     TestLabel "translateQMeas_QVar_CVar_State_1" test26,
                                     TestLabel "translateQMeas_QVar_CVar_State_2" test27,
                                     TestLabel "translateQMeas_QVar_CReg_Gates" test28,
                                     TestLabel "translateQMeas_QVar_CReg_State_1" test29,
                                     TestLabel "translateQMeas_QVar_CReg_State_2" test30,
                                     TestLabel "translateQMeas_QReg_CVar_Gates" test31,
                                     TestLabel "translateQMeas_QReg_CVar_State_1" test32,
                                     TestLabel "translateQMeas_QReg_CVar_State_2" test33,
                                     TestLabel "translateQMeas_QReg_CReg_Gates" test34,
                                     TestLabel "translateQMeas_QReg_CReg_State_1" test35,
                                     TestLabel "translateQMeas_QReg_CReg_State_2" test36]

main = defaultMain tests
