module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.AST
import LinguaQuanta.Qasm.Operand

-----------------------------------------------------------------------------------------
-- assign

rval1 = QuipMeasure $ QRef "qubit1"
rval2 = QuipMeasure $ Cell "qubit2" 5

test1 = TestCase (assertEqual "assign works with QRef on the left-hand side (1/2)."
                              (AstAssign "decl1" Nothing rval1)
                              (assign (QRef "decl1") rval1))

test2 = TestCase (assertEqual "assign works with QRef on the left-hand side (2/2)."
                              (AstAssign "decl1" Nothing rval2)
                              (assign (QRef "decl1") rval2))

test3 = TestCase (assertEqual "assign works with Cell on the left-hand side (1/2)."
                              (AstAssign "decl2" (Just 6) rval1)
                              (assign (Cell "decl2" 6) rval1))

test4 = TestCase (assertEqual "assign works with Cell on the left-hand side (2/2)."
                              (AstAssign "decl2" (Just 8) rval2)
                              (assign (Cell "decl2" 8) rval2))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "assign_LHS_QRef_1" test1,
                                     TestLabel "assign_LHS_QRef_2" test2,
                                     TestLabel "assign_LHS_Cell_1" test3,
                                     TestLabel "assign_LHS_Cell_2" test4]

main = defaultMain tests
