module Main where

import qualified Data.IntMap.Strict as IntMap
import Data.Maybe
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.Gate
import LinguaQuanta.Quip.Wire
import LinguaQuanta.QuipToQasm.Wire

-----------------------------------------------------------------------------------------
-- Tests allocation and lookup

mixedWires = [(1, QWire),
              (5, QWire),
              (3, CWire),
              (9, CWire),
              (23, QWire),
              (50, QWire),
              (44, CWire)]

allocEmpty = allocateInputWires IntMap.empty
allocMixed = allocateInputWires $ IntMap.fromList mixedWires

test1 = TestCase (assertEqual "WireLookup retrieval from empty wire map (1/7)."
                              (Nothing :: Maybe Operand)
                              (getAllocation QWire 1 allocEmpty))

test2 = TestCase (assertEqual "WireLookup retrieval from empty wire map (2/7)."
                              (Nothing :: Maybe Operand)
                              (getAllocation QWire 5 allocEmpty))

test3 = TestCase (assertEqual "WireLookup retrieval from empty wire map (3/7)."
                              (Nothing :: Maybe Operand)
                              (getAllocation QWire 23 allocEmpty))

test4 = TestCase (assertEqual "WireLookup retrieval from empty wire map (4/7)."
                              (Nothing :: Maybe Operand)
                              (getAllocation QWire 50 allocEmpty))

test5 = TestCase (assertEqual "WireLookup retrieval from empty wire map (5/7)."
                              (Nothing :: Maybe Operand)
                              (getAllocation CWire 3 allocEmpty))

test6 = TestCase (assertEqual "WireLookup retrieval from empty wire map (6/7)."
                              (Nothing :: Maybe Operand)
                              (getAllocation CWire 9 allocEmpty))

test7 = TestCase (assertEqual "WireLookup retrieval from empty wire map (7/7)."
                              (Nothing :: Maybe Operand)
                              (getAllocation CWire 44 allocEmpty))

test8 = TestCase (assertEqual "WireLookup retrieval from empty wire map (1/7)."
                              expt
                              (getAllocation QWire 1 allocMixed))
    where expt = Just (Cell "input_qwires" 0) :: Maybe Operand

test9 = TestCase (assertEqual "WireLookup retrieval from empty wire map (2/7)."
                              expt
                              (getAllocation QWire 5 allocMixed))
    where expt = Just (Cell "input_qwires" 1) :: Maybe Operand

test10 = TestCase (assertEqual "WireLookup retrieval from empty wire map (3/7)."
                               expt
                               (getAllocation QWire 23 allocMixed))
    where expt = Just (Cell "input_qwires" 2) :: Maybe Operand

test11 = TestCase (assertEqual "WireLookup retrieval from empty wire map (4/7)."
                                expt
                               (getAllocation QWire 50 allocMixed))
    where expt = Just (Cell "input_qwires" 3) :: Maybe Operand

test12 = TestCase (assertEqual "WireLookup retrieval from empty wire map (5/7)."
                               expt
                               (getAllocation CWire 3 allocMixed))
    where expt = Just (Cell "input_cwires" 0) :: Maybe Operand

test13 = TestCase (assertEqual "WireLookup retrieval from empty wire map (6/7)."
                               expt
                               (getAllocation CWire 9 allocMixed))
    where expt = Just (Cell "input_cwires" 1) :: Maybe Operand

test14 = TestCase (assertEqual "WireLookup retrieval from empty wire map (7/7)."
                               expt
                               (getAllocation CWire 44 allocMixed))
    where expt = Just (Cell "input_cwires" 2) :: Maybe Operand

test15 = TestCase (assertEqual "WireLookup retrieval fails with invalid type."
                               (Nothing :: Maybe Operand)
                               (getAllocation CWire 1 allocMixed))

test16 = TestCase (assertEqual "WireLookup retrieval fails with invalid identifier."
                               (Nothing :: Maybe Operand)
                               (getAllocation QWire 256 allocMixed))

-----------------------------------------------------------------------------------------
-- getState: After allocation

test17 = TestCase (assertEqual "getState supports QWire allocations."
                               (Just QWire :: Maybe WireType)
                               (getState 1 allocMixed))

test18 = TestCase (assertEqual "getState supports CWire allocations."
                               (Just CWire :: Maybe WireType)
                               (getState 9 allocMixed))

test19 = TestCase (assertEqual "getState handles missing allocations."
                               (Nothing :: Maybe WireType)
                               (getState 256 allocMixed))

-----------------------------------------------------------------------------------------
-- Tests state collapse

Left (collapsedMap1, collapsedName1) = collapseState 5 allocMixed
Left (collapsedMap2, collapsedName2) = collapseState 23 collapsedMap1

test20 = TestCase (assertEqual "collapseState returns the correct names (1/2)."
                               (Just "shadow_cwire_0" :: Maybe String)
                               collapsedName1)

test21 = TestCase (assertEqual "collapseState returns the correct names (2/2)."
                               (Just "shadow_cwire_1" :: Maybe String)
                               collapsedName2)

test22 = TestCase (assertEqual "collapseState updates decalarations correctly (1/3)."
                               "shadow_cwire_0"
                               name)
    where Just (QRef name) = getAllocation CWire 5 collapsedMap1

test23 = TestCase (assertEqual "collapseState updates decalarations correctly (2/3)."
                               "shadow_cwire_0"
                               name)
    where Just (QRef name) = getAllocation CWire 5 collapsedMap2

test24 = TestCase (assertEqual "collapseState updates decalarations correctly (3/3)."
                               "shadow_cwire_1"
                               name)
    where Just (QRef name) = getAllocation CWire 23 collapsedMap2

test25 = TestCase (assertEqual "collapseState will not collapse classical wires."
                               (BadWireState CWire)
                               err)
    where Right err = collapseState 5 collapsedMap2

test26 = TestCase (assertEqual "collapseState will reject unallocated wires."
                               (UnallocatedWire 100)
                               err)
    where Right err = collapseState 100 collapsedMap2

test27 = TestCase (assertEqual "collapseState leaves other wires unchanged (1/5)."
                               (Just QWire :: Maybe WireType)
                               (getState 1 collapsedMap2))

test28 = TestCase (assertEqual "collapseState leaves other wires unchanged (2/5)."
                               (Just CWire :: Maybe WireType)
                               (getState 3 collapsedMap2))

test29 = TestCase (assertEqual "collapseState leaves other wires unchanged (3/5)."
                               (Just CWire :: Maybe WireType)
                               (getState 9 collapsedMap2))

test30 = TestCase (assertEqual "collapseState leaves other wires unchanged (4/5)."
                               (Just QWire :: Maybe WireType)
                               (getState 50 collapsedMap2))

test31 = TestCase (assertEqual "collapseState leaves other wires unchanged (5/5)."
                               (Just CWire :: Maybe WireType)
                               (getState 44 collapsedMap2))

-----------------------------------------------------------------------------------------
-- Bit termination (and integration with state collapse)

Left termMap1 = termCBit 3 allocMixed
Left termMap2 = termQBit 23 allocMixed

test32 = TestCase (assertEqual "termCBit sets classical bits to inactive states."
                               (Nothing :: Maybe WireType)
                               (getState 3 termMap1))

test33 = TestCase (assertEqual "termQBit sets classical bits to inactive states."
                               (Nothing :: Maybe WireType)
                               (getState 23 termMap2))

test34 = TestCase (assertEqual "termBitByType leaves other wires unchanged (1/5)."
                               (Just QWire :: Maybe WireType)
                               (getState 1 termMap2))

test35 = TestCase (assertEqual "termBitByType leaves other wires unchanged (2/5)."
                               (Just QWire :: Maybe WireType)
                               (getState 5 termMap2))

test36 = TestCase (assertEqual "termBitByType leaves other wires unchanged (3/5)."
                               (Just CWire :: Maybe WireType)
                               (getState 9 termMap2))

test37 = TestCase (assertEqual "termBitByType leaves other wires unchanged (4/5)."
                               (Just QWire :: Maybe WireType)
                               (getState 50 termMap2))

test38 = TestCase (assertEqual "termBitByType leaves other wires unchanged (5/5)."
                               (Just CWire :: Maybe WireType)
                               (getState 44 termMap2))

test39 = TestCase (assertEqual "termCBit will not terminate quantum wires."
                               (BadWireState QWire)
                               err)
    where Right err = termCBit 5 termMap2

test40 = TestCase (assertEqual "termQBit will not terminate classical wires."
                               (BadWireState CWire)
                               err)
    where Right err = termQBit 44 termMap2

test41 = TestCase (assertEqual "termCBit will reject unallocated wires."
                               (UnallocatedWire 100)
                               err)
    where Right err = termCBit 100 collapsedMap2

test42 = TestCase (assertEqual "termQBit will reject unallocated wires."
                               (UnallocatedWire 100)
                               err)
    where Right err = termQBit 100 collapsedMap2

test43 = TestCase (assertEqual "collapseState will reject inactive wires."
                               WireInactive
                               err)
    where Right err = collapseState 23 termMap2

-----------------------------------------------------------------------------------------
-- Bit initialization (and integration with state collapse)

Left (initMap1, initName1) = initCBit 1000 allocMixed
Left (initMap2, initName2) = initCBit 1001 initMap1
Left (initMap3, initName3) = initQBit 2000 initMap2
Left (initMap4, initName4) = initQBit 2001 initMap3

test44 = TestCase (assertEqual "initCBit returns the correct names (1/2)."
                               (Just "shadow_cwire_0" :: Maybe String)
                               initName1)

test45 = TestCase (assertEqual "initCBit returns the correct names (2/2)."
                               (Just "shadow_cwire_1" :: Maybe String)
                               initName2)

test46 = TestCase (assertEqual "initQBit returns the correct names (1/2)."
                               (Just "shadow_qwire_2" :: Maybe String)
                               initName3)

test47 = TestCase (assertEqual "initQBit returns the correct names (2/2)."
                               (Just "shadow_qwire_3" :: Maybe String)
                               initName4)

test48 = TestCase (assertEqual "initCBit sets decl to CWire state (1/2)."
                               (Just CWire :: Maybe WireType)
                               (getState 1000 initMap1))

test49 = TestCase (assertEqual "initCBit sets decl to CWire state (2/2)."
                               (Just CWire :: Maybe WireType)
                               (getState 1001 initMap2))

test50 = TestCase (assertEqual "initQBit sets decl to QWire state (1/2)."
                               (Just QWire :: Maybe WireType)
                               (getState 2000 initMap3))

test51 = TestCase (assertEqual "initQBit sets decl to QWire state (2/2)."
                               (Just QWire :: Maybe WireType)
                               (getState 2001 initMap4))

test52 = TestCase (assertEqual "initBitByType leaves other wires unchanged (1/7)."
                               (Just QWire :: Maybe WireType)
                               (getState 1 initMap4))

test53 = TestCase (assertEqual "initBitByType leaves other wires unchanged (2/7)."
                               (Just QWire :: Maybe WireType)
                               (getState 5 initMap4))

test54 = TestCase (assertEqual "initBitByType leaves other wires unchanged (3/7)."
                               (Just CWire :: Maybe WireType)
                               (getState 3 initMap4))

test55 = TestCase (assertEqual "initBitByType leaves other wires unchanged (4/7)."
                               (Just CWire :: Maybe WireType)
                               (getState 9 initMap4))

test56 = TestCase (assertEqual "initBitByType leaves other wires unchanged (5/7)."
                               (Just QWire :: Maybe WireType)
                               (getState 23 initMap4))

test57 = TestCase (assertEqual "initBitByType leaves other wires unchanged (6/7)."
                               (Just QWire :: Maybe WireType)
                               (getState 50 initMap4))

test58 = TestCase (assertEqual "initBitByType leaves other wires unchanged (7/7)."
                               (Just CWire :: Maybe WireType)
                               (getState 44 initMap4))

test59 = TestCase (assertEqual "initCBit will reject double initializations."
                               (DoubleInit 1000)
                               err)
    where Right err = initCBit 1000 initMap4

test60 = TestCase (assertEqual "initQBit will reject double initializations."
                               (DoubleInit 2000)
                               err)
    where Right err = initQBit 2000 initMap4

Left (reinitMap1, reinitName1) = initCBit 3 termMap1
Left (reinitMap2, reinitName2) = initCBit 23 termMap2
Left (reinitMap3, reinitName3) = initQBit 3 termMap1
Left (reinitMap4, reinitName4) = initQBit 23 termMap2

test61 = TestCase (assertEqual "initCBit returns no name on cbit reinitialization (1/2)."
                               (Nothing :: Maybe String)
                               reinitName1)

test62 = TestCase (assertEqual "initCBit returns no name on cbit reinitialization (2/2)."
                               (Just "shadow_cwire_0" :: Maybe String)
                               reinitName2)

test63 = TestCase (assertEqual "initCBit returns no name on cbit reinitialization (1/2)."
                               (Just "shadow_qwire_0" :: Maybe String)
                               reinitName3)

test64 = TestCase (assertEqual "initCBit returns no name on cbit reinitialization (2/2)."
                               (Nothing :: Maybe String)
                               reinitName4)

Left measReinitName1                   = termCBit 3 allocMixed
Left (measReinitMap2, measReinitName2) = initQBit 3 measReinitName1
Left (measReinitMap3, measReinitName3) = collapseState 3 measReinitMap2

test65 = TestCase (assertEqual "collapseState reuses existing allocations whne possible."
                               (Nothing :: Maybe String)
                               measReinitName3)

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "Empty_Allocation_1" test1,
                                     TestLabel "Empty_Allocation_2" test2,
                                     TestLabel "Empty_Allocation_3" test3,
                                     TestLabel "Empty_Allocation_4" test4,
                                     TestLabel "Empty_Allocation_5" test5,
                                     TestLabel "Empty_Allocation_6" test6,
                                     TestLabel "Empty_Allocation_7" test7,
                                     TestLabel "Mixed_Allocation_1" test8,
                                     TestLabel "Mixed_Allocation_2" test9,
                                     TestLabel "Mixed_Allocation_3" test10,
                                     TestLabel "Mixed_Allocation_4" test11,
                                     TestLabel "Mixed_Allocation_5" test12,
                                     TestLabel "Mixed_Allocation_6" test13,
                                     TestLabel "Mixed_Allocation_7" test14,
                                     TestLabel "Mixed_Allocation_BadType" test15,
                                     TestLabel "Mixed_Allocation_BadID" test16,
                                     TestLabel "getState_QWire" test17,
                                     TestLabel "getState_CWire" test18,
                                     TestLabel "getState_BadID" test19,
                                     TestLabel "collapseState_Names_1" test20,
                                     TestLabel "collapseState_Names_2" test21,
                                     TestLabel "collapseState_Allocs_1" test22,
                                     TestLabel "collapseState_Allocs_2" test23,
                                     TestLabel "collapseState_Allocs_3" test24,
                                     TestLabel "collapseState_BadType" test25,
                                     TestLabel "collapseState_NoAlloc" test26,
                                     TestLabel "collapseState_Untouched_1" test27,
                                     TestLabel "collapseState_Untouched_2" test28,
                                     TestLabel "collapseState_Untouched_3" test29,
                                     TestLabel "collapseState_Untouched_4" test30,
                                     TestLabel "collapseState_Untouched_5" test31,
                                     TestLabel "termCBit_Valid" test32,
                                     TestLabel "termQBit_Valid" test33,
                                     TestLabel "termBitByType_Untouched_1" test34,
                                     TestLabel "termBitByType_Untouched_2" test35,
                                     TestLabel "termBitByType_Untouched_3" test36,
                                     TestLabel "termBitByType_Untouched_4" test37,
                                     TestLabel "termBitByType_Untouched_5" test38,
                                     TestLabel "termCBit_TypeCheck" test39,
                                     TestLabel "termQBit_TypeCheck" test40,
                                     TestLabel "termCBit_AllocCheck" test41,
                                     TestLabel "termQBit_AllocCheck" test42,
                                     TestLabel "collapseState_HandleInactive" test43,
                                     TestLabel "initCBit_Name_1" test44,
                                     TestLabel "initCBit_Name_2" test45,
                                     TestLabel "initQBit_Name_1" test46,
                                     TestLabel "initQBit_Name_2" test47,
                                     TestLabel "initCBit_State_1" test48,
                                     TestLabel "initCBit_State_2" test49,
                                     TestLabel "initQBit_State_1" test50,
                                     TestLabel "initQBit_State_2" test51,
                                     TestLabel "initBitByType_Untouched_1" test52,
                                     TestLabel "initBitByType_Untouched_2" test53,
                                     TestLabel "initBitByType_Untouched_3" test54,
                                     TestLabel "initBitByType_Untouched_4" test55,
                                     TestLabel "initBitByType_Untouched_5" test56,
                                     TestLabel "initBitByType_Untouched_6" test57,
                                     TestLabel "initBitByType_Untouched_7" test58,
                                     TestLabel "initCBit_DoubleInit" test59,
                                     TestLabel "initQBit_DoubleInit" test60,
                                     TestLabel "initCBit_Reinit_1" test61,
                                     TestLabel "initCBit_Reinit_2" test62,
                                     TestLabel "initQBit_Reinit_1" test63,
                                     TestLabel "initQBit_Reinit_2" test64,
                                     TestLabel "collapseState_Unuse" test65]

main = defaultMain tests
