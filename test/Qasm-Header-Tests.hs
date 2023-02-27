module Main where

import Data.Maybe
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.Header

-----------------------------------------------------------------------------------------
-- toQasmHeader

(Just legacyDefault) = toQasmHeader "2.0"
(Just newestDefault) = toQasmHeader "3"

test1 = TestCase (assertBool "toQasmHeader has correct result on 2.0 (isLegacy)."
                             (isLegacy legacyDefault))

test2 = TestCase (assertBool "toQasmHeader has correct result on 2.0 (usingStd)."
                             (not $ usingStd legacyDefault))

test3 = TestCase (assertBool "toQasmHeader has correct result on 2.0 (usingBkp)."
                             (not $ usingBkp legacyDefault))

test4 = TestCase (assertBool "toQasmHeader has correct result on 2.0 (usingQpr)."
                             (not $ usingQpr legacyDefault))

test5 = TestCase (assertBool "toQasmHeader has correct result on 2.0 (usingQfn)."
                             (not $ usingQfn legacyDefault))

test6 = TestCase (assertBool "toQasmHeader has correct result on 3 (isLegacy)."
                             (not $ isLegacy newestDefault))

test7 = TestCase (assertBool "toQasmHeader has correct result on 3 (usingStd)."
                             (not $ usingStd newestDefault))

test8 = TestCase (assertBool "toQasmHeader has correct result on 3 (usingBkp)."
                             (not $ usingBkp newestDefault))

test9 = TestCase (assertBool "toQasmHeader has correct result on 3 (usingQpr)."
                             (not $ usingQpr newestDefault))

test10 = TestCase (assertBool "toQasmHeader has correct result on 3 (usingQfn)."
                              (not $ usingQfn newestDefault))

test11 = TestCase (assertBool "toQasmHeader rejects invalid versions."
                              (isNothing $ toQasmHeader "BadVersion"))

-----------------------------------------------------------------------------------------
-- addLib: dependencies on version.

test12 = TestCase (assertEqual "toQasmHeader checks legacy dependencies (1/4)."
                               (addLib lib legacyDefault)
                               (Right (NonLegacyLib lib) :: Either QasmHeader LibErr))
    where lib = "stdgates.inc"

test13 = TestCase (assertEqual "toQasmHeader checks legacy dependencies (2/4)."
                               (addLib lib legacyDefault)
                               (Right (NonLegacyLib lib) :: Either QasmHeader LibErr))
    where lib = "quipfuncs.inc"

test14 = TestCase (assertEqual "toQasmHeader checks legacy dependencies (3/4)."
                               (addLib lib newestDefault)
                               (Right (LegacyLib lib) :: Either QasmHeader LibErr))
    where lib = "qelib1.inc"

test15 = TestCase (assertEqual "toQasmHeader checks legacy dependencies (4/4)."
                               (addLib lib newestDefault)
                               (Right (LegacyLib lib) :: Either QasmHeader LibErr))
    where lib = "bkpgates.inc"

-----------------------------------------------------------------------------------------
-- addLib: dependencies on standard library.

test16 = TestCase (assertEqual "toQasmHeader checks stdlib dependencies (1/4)."
                               (addLib lib legacyDefault)
                               (Right err :: Either QasmHeader LibErr))
    where lib = "quipgates.inc"
          err = MissingDep lib "qelib1.inc"

test17 = TestCase (assertEqual "toQasmHeader checks stdlib dependencies (2/4)."
                               (addLib lib legacyDefault)
                               (Right err :: Either QasmHeader LibErr))
    where lib = "bkpgates.inc"
          err = MissingDep lib "qelib1.inc"

test18 = TestCase (assertEqual "toQasmHeader checks stdlib dependencies (3/4)."
                               (addLib lib newestDefault)
                               (Right err :: Either QasmHeader LibErr))
    where lib = "quipgates.inc"
          err = MissingDep lib "stdgates.inc"

test19 = TestCase (assertEqual "toQasmHeader checks stdlib dependencies (4/4)."
                               (addLib lib newestDefault)
                               (Right err :: Either QasmHeader LibErr))
    where lib = "quipfuncs.inc"
          err = MissingDep lib "stdgates.inc"

-----------------------------------------------------------------------------------------
-- addLib: importing standard library.

(Left legacyStdlib) = addLib "qelib1.inc" legacyDefault
(Left newestStdlib) = addLib "stdgates.inc" newestDefault

test20 = TestCase (assertBool "addLib for standard library in 2.0 (isLegacy)."
                              (isLegacy legacyStdlib))

test21 = TestCase (assertBool "addLib for standard library in 2.0 (usingStd)."
                              (usingStd legacyStdlib))

test22 = TestCase (assertBool "addLib for standard library in 2.0 (usingBkp)."
                              (not $ usingBkp legacyStdlib))

test23 = TestCase (assertBool "addLib for standard library in 2.0 (usingQpr)."
                              (not $ usingQpr legacyStdlib))

test24 = TestCase (assertBool "addLib for standard library in 2.0 (usingQfn)."
                              (not $ usingQfn legacyStdlib))

test25 = TestCase (assertBool "addLib for standard library in 3 (isLegacy)."
                              (not $ isLegacy newestStdlib))

test26 = TestCase (assertBool "addLib for standard library in 3 (usingStd)."
                              (usingStd newestStdlib))

test27 = TestCase (assertBool "addLib for standard library in 3 (usingBkp)."
                              (not $ usingBkp newestStdlib))

test28 = TestCase (assertBool "addLib for standard library in 3 (usingQpr)."
                              (not $ usingQpr newestStdlib))

test29 = TestCase (assertBool "addLib for standard library in 3 (usingQfn)."
                              (not $ usingQfn newestStdlib))

-----------------------------------------------------------------------------------------
-- addLib: importing libraries after standard library.

(Left legacyGates) = addLib "quipgates.inc" legacyStdlib
(Left legacyBackp) = addLib "bkpgates.inc"  legacyStdlib
(Left newestGates) = addLib "quipgates.inc" newestStdlib
(Left newestFuncs) = addLib "quipfuncs.inc" newestStdlib

test30 = TestCase (assertBool "addLib for quipgates.inc in 2.0 (isLegacy)."
                              (isLegacy legacyGates))

test31 = TestCase (assertBool "addLib for quipgates.inc in 2.0 (usingStd)."
                              (usingStd legacyGates))

test32 = TestCase (assertBool "addLib for quipgates.inc in 2.0 (usingBkp)."
                              (not $ usingBkp legacyGates))

test33 = TestCase (assertBool "addLib for quipgates.inc in 2.0 (usingQpr)."
                              (usingQpr legacyGates))

test34 = TestCase (assertBool "addLib for quipgates.inc in 2.0 (usingQfn)."
                              (not $ usingQfn legacyGates))

test35 = TestCase (assertBool "addLib for bkpgates.inc in 2.0 (isLegacy)."
                              (isLegacy legacyBackp))

test36 = TestCase (assertBool "addLib for bkpgates.inc in 2.0 (usingStd)."
                              (usingStd legacyBackp))

test37 = TestCase (assertBool "addLib for bkpgates.inc in 2.0 (usingBkp)."
                              (usingBkp legacyBackp))

test38 = TestCase (assertBool "addLib for bkpgates.inc in 2.0 (usingQpr)."
                              (not $ usingQpr legacyBackp))

test39 = TestCase (assertBool "addLib for bkpgates.inc in 2.0 (usingQfn)."
                              (not $ usingQfn legacyBackp))

test40 = TestCase (assertBool "addLib for quipgates.inc in 3 (isLegacy)."
                              (not $ isLegacy newestGates))

test41 = TestCase (assertBool "addLib for quipgates.inc in 3 (usingStd)."
                              (usingStd newestGates))

test42 = TestCase (assertBool "addLib for quipgates.inc in 3 (usingBkp)."
                              (not $ usingBkp newestGates))

test43 = TestCase (assertBool "addLib for quipgates.inc in 3 (usingQpr)."
                              (usingQpr newestGates))

test44 = TestCase (assertBool "addLib for quipgates.inc in 3 (usingQfn)."
                              (not $ usingQfn newestGates))

test45 = TestCase (assertBool "addLib for quipfuncs.inc in 3 (isLegacy)."
                              (not $ isLegacy newestFuncs))

test46 = TestCase (assertBool "addLib for quipfuncs.inc in 3 (usingStd)."
                              (usingStd newestFuncs))

test47 = TestCase (assertBool "addLib for quipfuncs.inc in 3 (usingBkp)."
                              (not $ usingBkp newestFuncs))

test48 = TestCase (assertBool "addLib for quipfuncs.inc in 3 (usingQpr)."
                              (not $ usingQpr newestFuncs))

test49 = TestCase (assertBool "addLib for quipfuncs.inc in 3 (usingQfn)."
                              (usingQfn newestFuncs))

-----------------------------------------------------------------------------------------
-- addLib: imports commute (there are finitely many states without arbitrary imports).

(Left legacyOpt1) = addLib "quipgates.inc" legacyBackp
(Left legacyOpt2) = addLib "bkpgates.inc"  legacyGates
(Left newestOpt1) = addLib "quipgates.inc" newestFuncs
(Left newestOpt2) = addLib "quipfuncs.inc" newestGates

test50 = TestCase (assertBool "addLib for all includes in 2.0 (isLegacy)."
                              (isLegacy legacyOpt1))

test51 = TestCase (assertBool "addLib for all includes in 2.0 (usingStd)."
                              (usingStd legacyOpt1))

test52 = TestCase (assertBool "addLib for all includes in 2.0 (usingBkp)."
                              (usingBkp legacyOpt1))

test53 = TestCase (assertBool "addLib for all includes in 2.0 (usingQpr)."
                              (usingQpr legacyOpt1))

test54 = TestCase (assertBool "addLib for all includes in 2.0 (usingQfn)."
                              (not $ usingQfn legacyOpt1))

test55 = TestCase (assertEqual "addLib commutes in 2.0."
                               legacyOpt1
                               legacyOpt2)

test56 = TestCase (assertBool "addLib for all includes in 3 (isLegacy)."
                              (not $ isLegacy newestOpt1))

test57 = TestCase (assertBool "addLib for all includes in 3 (usingStd)."
                              (usingStd newestOpt1))

test58 = TestCase (assertBool "addLib for all includes in 3 (usingBkp)."
                              (not $ usingBkp newestOpt1))

test59 = TestCase (assertBool "addLib for all includes in 3 (usingQpr)."
                              (usingQpr newestOpt1))

test60 = TestCase (assertBool "addLib for all includes in 3 (usingQfn)."
                              (usingQfn newestOpt1))

test61 = TestCase (assertEqual "addLib commutes in 3."
                               newestOpt1
                               newestOpt2)

-----------------------------------------------------------------------------------------
-- addLib: double imports fail.

test62 = TestCase (assertEqual "addLib rejects double insertions (1/16)."
                               (addLib lib legacyStdlib)
                               (Right err :: Either QasmHeader LibErr))
    where lib = "qelib1.inc"
          err = DoubleImport lib

test63 = TestCase (assertEqual "addLib rejects double insertions (2/16)."
                               (addLib lib newestStdlib)
                               (Right err :: Either QasmHeader LibErr))
    where lib = "stdgates.inc"
          err = DoubleImport lib

test64 = TestCase (assertEqual "addLib rejects double insertions (3/16)."
                               (addLib lib legacyGates)
                               (Right err :: Either QasmHeader LibErr))
    where lib = "qelib1.inc"
          err = DoubleImport lib

test65 = TestCase (assertEqual "addLib rejects double insertions (4/16)."
                               (addLib lib legacyGates)
                               (Right err :: Either QasmHeader LibErr))
    where lib = "quipgates.inc"
          err = DoubleImport lib

test66 = TestCase (assertEqual "addLib rejects double insertions (5/16)."
                               (addLib lib legacyBackp)
                               (Right err :: Either QasmHeader LibErr))
    where lib = "qelib1.inc"
          err = DoubleImport lib

test67 = TestCase (assertEqual "addLib rejects double insertions (6/16)."
                               (addLib lib legacyBackp)
                               (Right err :: Either QasmHeader LibErr))
    where lib = "bkpgates.inc"
          err = DoubleImport lib

test68 = TestCase (assertEqual "addLib rejects double insertions (7/16)."
                               (addLib lib newestGates)
                               (Right err :: Either QasmHeader LibErr))
    where lib = "stdgates.inc"
          err = DoubleImport lib

test69 = TestCase (assertEqual "addLib rejects double insertions (8/16)."
                               (addLib lib newestGates)
                               (Right err :: Either QasmHeader LibErr))
    where lib = "quipgates.inc"
          err = DoubleImport lib

test70 = TestCase (assertEqual "addLib rejects double insertions (9/16)."
                               (addLib lib newestFuncs)
                               (Right err :: Either QasmHeader LibErr))
    where lib = "stdgates.inc"
          err = DoubleImport lib

test71 = TestCase (assertEqual "addLib rejects double insertions (10/16)."
                               (addLib lib newestFuncs)
                               (Right err :: Either QasmHeader LibErr))
    where lib = "quipfuncs.inc"
          err = DoubleImport lib

test72 = TestCase (assertEqual "addLib rejects double insertions (11/16)."
                               (addLib lib legacyOpt1)
                               (Right err :: Either QasmHeader LibErr))
    where lib = "qelib1.inc"
          err = DoubleImport lib

test73 = TestCase (assertEqual "addLib rejects double insertions (12/16)."
                               (addLib lib legacyOpt1)
                               (Right err :: Either QasmHeader LibErr))
    where lib = "quipgates.inc"
          err = DoubleImport lib

test74 = TestCase (assertEqual "addLib rejects double insertions (13/16)."
                               (addLib lib legacyOpt1)
                               (Right err :: Either QasmHeader LibErr))
    where lib = "bkpgates.inc"
          err = DoubleImport lib

test75 = TestCase (assertEqual "addLib rejects double insertions (14/16)."
                               (addLib lib newestOpt1)
                               (Right err :: Either QasmHeader LibErr))
    where lib = "stdgates.inc"
          err = DoubleImport lib

test76 = TestCase (assertEqual "addLib rejects double insertions (15/16)."
                               (addLib lib newestOpt1)
                               (Right err :: Either QasmHeader LibErr))
    where lib = "quipgates.inc"
          err = DoubleImport lib

test77 = TestCase (assertEqual "addLib rejects double insertions (16/16)."
                               (addLib lib newestOpt1)
                               (Right err :: Either QasmHeader LibErr))
    where lib = "quipfuncs.inc"
          err = DoubleImport lib

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "toQasmHeader_v2_1" test1,
                                     TestLabel "toQasmHeader_v2_2" test2,
                                     TestLabel "toQasmHeader_v2_3" test3,
                                     TestLabel "toQasmHeader_v2_4" test4,
                                     TestLabel "toQasmHeader_v2_5" test5,
                                     TestLabel "toQasmHeader_v3_1" test6,
                                     TestLabel "toQasmHeader_v3_2" test7,
                                     TestLabel "toQasmHeader_v3_3" test8,
                                     TestLabel "toQasmHeader_v3_4" test9,
                                     TestLabel "toQasmHeader_v3_5" test10,
                                     TestLabel "toQasmHeader_BadVersion" test11,
                                     TestLabel "addLib_LegacyReqs_1" test12,
                                     TestLabel "addLib_LegacyReqs_2" test13,
                                     TestLabel "addLib_LegacyReqs_3" test14,
                                     TestLabel "addLib_LegacyReqs_4" test15,
                                     TestLabel "addLib_StdlibReqs_1" test16,
                                     TestLabel "addLib_StdlibReqs_2" test17,
                                     TestLabel "addLib_StdlibReqs_3" test18,
                                     TestLabel "addLib_StdlibReqs_4" test19,
                                     TestLabel "addLib_Stdlib_v2_1" test20,
                                     TestLabel "addLib_Stdlib_v2_2" test21,
                                     TestLabel "addLib_Stdlib_v2_3" test22,
                                     TestLabel "addLib_Stdlib_v2_4" test23,
                                     TestLabel "addLib_Stdlib_v2_5" test24,
                                     TestLabel "addLib_Stdlib_v3_1" test25,
                                     TestLabel "addLib_Stdlib_v3_2" test26,
                                     TestLabel "addLib_Stdlib_v3_3" test27,
                                     TestLabel "addLib_Stdlib_v3_4" test28,
                                     TestLabel "addLib_Stdlib_v3_5" test29,
                                     TestLabel "addLib_QuipGates_v2_1" test30,
                                     TestLabel "addLib_QuipGates_v2_2" test31,
                                     TestLabel "addLib_QuipGates_v2_3" test32,
                                     TestLabel "addLib_QuipGates_v2_4" test33,
                                     TestLabel "addLib_QuipGates_v2_5" test34,
                                     TestLabel "addLib_BkpGates_v2_1" test35,
                                     TestLabel "addLib_BkpGates_v2_2" test36,
                                     TestLabel "addLib_BkpGates_v2_3" test37,
                                     TestLabel "addLib_BkpGates_v2_4" test38,
                                     TestLabel "addLib_BkpGates_v2_5" test39,
                                     TestLabel "addLib_QuipGates_v3_1" test40,
                                     TestLabel "addLib_QuipGates_v3_2" test41,
                                     TestLabel "addLib_QuipGates_v3_3" test42,
                                     TestLabel "addLib_QuipGates_v3_4" test43,
                                     TestLabel "addLib_QuipGates_v3_5" test44,
                                     TestLabel "addLib_QuipFuncs_v3_1" test45,
                                     TestLabel "addLib_QuipFuncs_v3_2" test46,
                                     TestLabel "addLib_QuipFuncs_v3_3" test47,
                                     TestLabel "addLib_QuipFuncs_v3_4" test48,
                                     TestLabel "addLib_QuipFuncs_v3_5" test49,
                                     TestLabel "addLib_All_v2_1" test50,
                                     TestLabel "addLib_All_v2_2" test51,
                                     TestLabel "addLib_All_v2_3" test52,
                                     TestLabel "addLib_All_v2_4" test53,
                                     TestLabel "addLib_All_v2_5" test54,
                                     TestLabel "addLib_All_v2_6" test55,
                                     TestLabel "addLib_All_v3_1" test56,
                                     TestLabel "addLib_All_v3_2" test57,
                                     TestLabel "addLib_All_v3_3" test58,
                                     TestLabel "addLib_All_v3_4" test59,
                                     TestLabel "addLib_All_v3_5" test60,
                                     TestLabel "addLib_All_v3_6" test61,
                                     TestLabel "addLib_DoubleAdd_1" test62,
                                     TestLabel "addLib_DoubleAdd_2" test63,
                                     TestLabel "addLib_DoubleAdd_3" test64,
                                     TestLabel "addLib_DoubleAdd_4" test65,
                                     TestLabel "addLib_DoubleAdd_5" test66,
                                     TestLabel "addLib_DoubleAdd_6" test67,
                                     TestLabel "addLib_DoubleAdd_7" test68,
                                     TestLabel "addLib_DoubleAdd_8" test69,
                                     TestLabel "addLib_DoubleAdd_9" test70,
                                     TestLabel "addLib_DoubleAdd_10" test71,
                                     TestLabel "addLib_DoubleAdd_11" test72,
                                     TestLabel "addLib_DoubleAdd_12" test73,
                                     TestLabel "addLib_DoubleAdd_13" test74,
                                     TestLabel "addLib_DoubleAdd_14" test75,
                                     TestLabel "addLib_DoubleAdd_15" test76,
                                     TestLabel "addLib_DoubleAdd_16" test77]

main = defaultMain tests
