module Main where

import qualified Data.IntMap.Strict as IntMap
import Data.Maybe
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import LinguaQuanta.Qasm.Expression
import LinguaQuanta.Qasm.Gate
  ( Operand(..)
  , addCtrlsToMod
  , addNegCtrlsToMod
  , negateMod
  , nullGateMod
  )
import qualified LinguaQuanta.Qasm.GateName as Qasm
import LinguaQuanta.Qasm.Language
import LinguaQuanta.QasmToQuip.Gate
import LinguaQuanta.QasmToQuip.Wire
  ( initialAllocations
  , allocate
  )
import LinguaQuanta.Quip.Gate
import qualified LinguaQuanta.Quip.GateName as Quip
import LinguaQuanta.Quip.Wire

-----------------------------------------------------------------------------------------
-- Context

-- Allocations:
alloc0 = initialAllocations
alloc1 = fromJust $ allocate QWire "decl1" Nothing  alloc0
alloc2 = fromJust $ allocate CWire "decl2" (Just 5) alloc1
alloc3 = fromJust $ allocate QWire "decl3" (Just 4) alloc2
alloc4 = fromJust $ allocate CWire "decl4" Nothing  alloc3

-- Operands:
decl1       = QRef "decl1"
decl2at idx = Cell "decl2" idx
decl3at idx = Cell "decl3" idx
decl4       = QRef "decl4"

-- Modifiers:
mod0 = nullGateMod
mod1 = negateMod $ nullGateMod
mod2 = addCtrlsToMod 2 $ nullGateMod
mod3 = addCtrlsToMod 1 $ addNegCtrlsToMod 1 $ nullGateMod
mod4 = negateMod $ mod2
mod5 = negateMod $ mod3

-----------------------------------------------------------------------------------------
-- Translating the S gate (many configurations)

test1 = TestCase (assertEqual "Translation of an S gate without modifiers."
                              [NamedGate Quip.GateS False [0] []]
                              (namedGateTransl alloc4 Qasm.GateS [decl1] mod0))

test2 = TestCase (assertEqual "Translation of an S gate with inversion."
                              [NamedGate Quip.GateS True [0] []]
                              (namedGateTransl alloc4 Qasm.GateS [decl1] mod1))

test3 = TestCase (assertEqual "Translation of an S gate with 2 ctrls (1/2)."
                              [NamedGate Quip.GateS False ins ctrls]
                              (namedGateTransl alloc4 Qasm.GateS ops mod2))
    where ops   = [decl1, decl2at 0, decl3at 3]
          ins   = [9]
          ctrls = [Pos 0, Pos 1]

test4 = TestCase (assertEqual "Translation of an S gate with 2 ctrls (2/2)."
                              [NamedGate Quip.GateS False ins ctrls]
                              (namedGateTransl alloc4 Qasm.GateS ops mod3))
    where ops   = [decl1, decl2at 0, decl3at 3]
          ins   = [9]
          ctrls = [Pos 0, Neg 1]

test5 = TestCase (assertEqual "Translation of an S gate with 2 ctrls and inversion (1/2)."
                              [NamedGate Quip.GateS True ins ctrls]
                              (namedGateTransl alloc4 Qasm.GateS ops mod4))
    where ops   = [decl1, decl2at 0, decl3at 3]
          ins   = [9]
          ctrls = [Pos 0, Pos 1]

test6 = TestCase (assertEqual "Translation of an S gate with 2 ctrls and inversion (2/2)."
                              [NamedGate Quip.GateS True ins ctrls]
                              (namedGateTransl alloc4 Qasm.GateS ops mod5))
    where ops   = [decl1, decl2at 0, decl3at 3]
          ins   = [9]
          ctrls = [Pos 0, Neg 1]

-----------------------------------------------------------------------------------------
-- Translating simple named gates (single configuration)

test7 = TestCase (assertEqual "Translation of a Pauli-X gate without modifiers."
                              [NamedGate Quip.GateX False [0] []]
                              (namedGateTransl alloc4 Qasm.GateX [decl1] mod0))

test8 = TestCase (assertEqual "Translation of a Pauli-Y gate without modifiers."
                              [NamedGate Quip.GateY False [0] []]
                              (namedGateTransl alloc4 Qasm.GateY [decl1] mod0))

test9 = TestCase (assertEqual "Translation of a Pauli-Z gate without modifiers."
                              [NamedGate Quip.GateZ False [0] []]
                              (namedGateTransl alloc4 Qasm.GateZ [decl1] mod0))

test10 = TestCase (assertEqual "Translation of a Hadamard gate without modifiers."
                               [NamedGate Quip.GateH False [0] []]
                               (namedGateTransl alloc4 Qasm.GateH [decl1] mod0))

test11 = TestCase (assertEqual "Translation of a Swap gate without modifiers."
                               [NamedGate Quip.GateSwap False [0, 4] []]
                               (namedGateTransl alloc4 Qasm.GateSwap ops mod0))
    where ops = [decl1, decl2at 3]

test12 = TestCase (assertEqual "Translation of an SX gate without modifiers."
                               [NamedGate Quip.GateV False [0] []]
                               (namedGateTransl alloc4 Qasm.GateSX [decl1] mod0))

test13 = TestCase (assertEqual "Translation of a T gate without modifiers."
                               [NamedGate Quip.GateT False [0] []]
                               (namedGateTransl alloc4 Qasm.GateT [decl1] mod0))

test14 = TestCase (assertEqual "Translation of an iX gate without modifiers."
                               [NamedGate Quip.GateIX False [0] []]
                               (namedGateTransl alloc4 Qasm.GateQuipIX [decl1] mod0))

test15 = TestCase (assertEqual "Translation of an omega gate without modifiers."
                               [NamedGate Quip.GateOmega False [0] []]
                               (namedGateTransl alloc4 Qasm.GateQuipOmega [decl1] mod0))

test16 = TestCase (assertEqual "Translation of an E gate without modifiers."
                               [NamedGate Quip.GateE False [0] []]
                               (namedGateTransl alloc4 Qasm.GateQuipE [decl1] mod0))

test17 = TestCase (assertEqual "Translation of a W gate without modifiers."
                               [NamedGate Quip.GateW False [0] []]
                               (namedGateTransl alloc4 Qasm.GateQuipW [decl1] mod0))

-----------------------------------------------------------------------------------------
-- Translating named inverse gates.

test18 = TestCase (assertEqual "Translation of an Sdg gate without modifiers."
                               [NamedGate Quip.GateS True [0] []]
                               (namedGateTransl alloc4 Qasm.GateSdg [decl1] mod0))

test19 = TestCase (assertEqual "Translation of an Sdg gate with inversion."
                               [NamedGate Quip.GateS False [0] []]
                               (namedGateTransl alloc4 Qasm.GateSdg [decl1] mod1))

test20 = TestCase (assertEqual "Translation of an Sdg gate with 2 ctrls (1/2)."
                               [NamedGate Quip.GateS True ins ctrls]
                               (namedGateTransl alloc4 Qasm.GateSdg ops mod2))
    where ops   = [decl1, decl2at 0, decl3at 3]
          ins   = [9]
          ctrls = [Pos 0, Pos 1]

test21 = TestCase (assertEqual "Translation of an Sdg gate with 2 ctrls (2/2)."
                               [NamedGate Quip.GateS True ins ctrls]
                               (namedGateTransl alloc4 Qasm.GateSdg ops mod3))
    where ops   = [decl1, decl2at 0, decl3at 3]
          ins   = [9]
          ctrls = [Pos 0, Neg 1]

test22 = TestCase (assertEqual "Translation of an Sdg gate with 2 ctrls and inversion (1/2)."
                               [NamedGate Quip.GateS False ins ctrls]
                               (namedGateTransl alloc4 Qasm.GateSdg ops mod4))
    where ops   = [decl1, decl2at 0, decl3at 3]
          ins   = [9]
          ctrls = [Pos 0, Pos 1]

test23 = TestCase (assertEqual "Translation of an Sdg gate with 2 ctrls and inversion (2/2)."
                               [NamedGate Quip.GateS False ins ctrls]
                               (namedGateTransl alloc4 Qasm.GateSdg ops mod5))
    where ops   = [decl1, decl2at 0, decl3at 3]
          ins   = [9]
          ctrls = [Pos 0, Neg 1]

test24 = TestCase (assertEqual "Translation of an Tdg gate without modifiers."
                               [NamedGate Quip.GateT True [0] []]
                               (namedGateTransl alloc4 Qasm.GateTdg [decl1] mod0))

-----------------------------------------------------------------------------------------
-- Translating the CX gate (many configurations)

test25 = TestCase (assertEqual "Translation of a CX gate without modifiers."
                               [NamedGate Quip.GateX False ins ctrls]
                               (namedGateTransl alloc4 Qasm.GateCX ops mod0))
    where ops   = [decl1, decl4]
          ins   = [10]
          ctrls = [Pos 0]

test26 = TestCase (assertEqual "Translation of a CX gate with inversion."
                               [NamedGate Quip.GateX False ins ctrls]
                               (namedGateTransl alloc4 Qasm.GateCX ops mod1))
    where ops   = [decl1, decl4]
          ins   = [10]
          ctrls = [Pos 0]

test27 = TestCase (assertEqual "Translation of a CX gate with 2 ctrls (1/2)."
                                [NamedGate Quip.GateX False ins ctrls]
                                (namedGateTransl alloc4 Qasm.GateCX ops mod2))
    where ops   = [decl2at 0, decl3at 3, decl1, decl4]
          ins   = [10]
          ctrls = [Pos 0, Pos 1, Pos 9]

test28 = TestCase (assertEqual "Translation of a CX gate with 2 ctrls (2/2)."
                                [NamedGate Quip.GateX False ins ctrls]
                                (namedGateTransl alloc4 Qasm.GateCX ops mod3))
    where ops   = [decl2at 0, decl3at 3, decl1, decl4]
          ins   = [10]
          ctrls = [Pos 0, Pos 1, Neg 9]

test29 = TestCase (assertEqual "Translation of a CX gate with 2 ctrls and inversion (1/2)."
                                [NamedGate Quip.GateX False ins ctrls]
                                (namedGateTransl alloc4 Qasm.GateCX ops mod4))
    where ops   = [decl2at 0, decl3at 3, decl1, decl4]
          ins   = [10]
          ctrls = [Pos 0, Pos 1, Pos 9]

test30 = TestCase (assertEqual "Translation of a CX gate with 2 ctrls and inversion (2/2)."
                                [NamedGate Quip.GateX False ins ctrls]
                                (namedGateTransl alloc4 Qasm.GateCX ops mod5))
    where ops   = [decl2at 0, decl3at 3, decl1, decl4]
          ins   = [10]
          ctrls = [Pos 0, Pos 1, Neg 9]

-----------------------------------------------------------------------------------------
-- Translating singly controlled gates (single configuration)

test31 = TestCase (assertEqual "Translation of a CY gate without modifiers."
                               [NamedGate Quip.GateY False ins ctrls]
                               (namedGateTransl alloc4 Qasm.GateCY ops mod0))
    where ops   = [decl1, decl4]
          ins   = [10]
          ctrls = [Pos 0]

test32 = TestCase (assertEqual "Translation of a CZ gate without modifiers."
                               [NamedGate Quip.GateZ False ins ctrls]
                               (namedGateTransl alloc4 Qasm.GateCZ ops mod0))
    where ops   = [decl1, decl4]
          ins   = [10]
          ctrls = [Pos 0]

test33 = TestCase (assertEqual "Translation of a CH gate without modifiers."
                               [NamedGate Quip.GateH False ins ctrls]
                               (namedGateTransl alloc4 Qasm.GateCH ops mod0))
    where ops   = [decl1, decl4]
          ins   = [10]
          ctrls = [Pos 0]

test34 = TestCase (assertEqual "Translation of a CSwap gate without modifiers."
                               [NamedGate Quip.GateSwap False ins ctrls]
                               (namedGateTransl alloc4 Qasm.GateCSwap ops mod0))
    where ops   = [decl1, decl4, decl2at 0]
          ins   = [10, 1]
          ctrls = [Pos 0]

-----------------------------------------------------------------------------------------
-- Translating the Toffoli gate (single configuration)

test35 = TestCase (assertEqual "Translation of a CCX gate with 2 ctrls and inversion."
                                [NamedGate Quip.GateX False ins ctrls]
                                (namedGateTransl alloc4 Qasm.GateCCX ops mod5))
    where ops   = [decl1, decl2at 0, decl2at 1, decl2at 2, decl2at 3]
          ins   = [4]
          ctrls = [Pos 2, Pos 3, Pos 0, Neg 1]

-----------------------------------------------------------------------------------------
-- Translating global phase gates

test36 = TestCase (assertEqual "Translation of global phase gates without controls."
                               [PhaseGate 5.0 []]
                               (translGPhase alloc4 param [] mod0))
    where param = Times Pi $ DecFloat "5.0"

test37 = TestCase (assertEqual "Translation of global phase gates with inversion."
                               [PhaseGate (-7.1) []]
                               (translGPhase alloc4 param [] mod1))
    where param = Times Pi $ DecFloat "7.1"

test38 = TestCase (assertEqual "Translation of global phase gates with controls."
                               [PhaseGate 0 ctrls]
                               (translGPhase alloc4 param ops mod2))
    where ops   = [decl1, decl2at 2]
          ctrls = [Pos 0, Pos 3]
          param = Times Pi $ DecFloat "0"

-----------------------------------------------------------------------------------------
-- Translating RZ gates (many configuration).

test39 = TestCase (assertEqual "Translation of an RZ gate (1/2)."
                               [RotGate Quip.RotExpZ False 0.5 [0] []]
                               (d1RotTransl alloc4 Qasm.GateRZ param [decl1] mod0))
    where param = DecInt "1"

test40 = TestCase (assertEqual "Translation of an RZ gate (2/2)."
                               [RotGate Quip.RotExpZ True 1.0 [2] []]
                               (d1RotTransl alloc4 Qasm.GateRZ param [decl2at 1] mod1))
    where param = DecFloat "2.0"

-----------------------------------------------------------------------------------------
-- Translating controlled RZ gates (many configurations).

test41 = TestCase (assertEqual "Translation of a controlled RZ gate (1/2)."
                               [RotGate Quip.RotExpZ False 0.5 [0] ctrls]
                               (d1RotTransl alloc4 Qasm.GateRZ param ops mod2))
    where ops   = [decl2at 1, decl2at 2, decl1]
          ctrls = [Pos 2, Pos 3]
          param = DecInt "1"

test42 = TestCase (assertEqual "Translation of a controlled RZ gate (2/2)."
                               [RotGate Quip.RotExpZ True 1.0 [0] ctrls]
                               (d1RotTransl alloc4 Qasm.GateRZ param ops mod5))
    where ops   = [decl2at 1, decl2at 2, decl1]
          ctrls = [Pos 2, Neg 3]
          param = DecFloat "2.0"

-----------------------------------------------------------------------------------------
-- Translating CRZ gates (many configuration).

test43 = TestCase (assertEqual "Translation of a CRZ gate (1/2)."
                               [RotGate Quip.RotExpZ False 0.5 [0] [Pos 2]]
                               (d1RotTransl alloc4 Qasm.GateCRZ param ops mod0))
    where ops   = [decl2at 1, decl1]
          param = DecInt "1"

test44 = TestCase (assertEqual "Translation of a CRZ gate (2/2)."
                               [RotGate Quip.RotExpZ True 1.0 [3] [Pos 4]]
                               (d1RotTransl alloc4 Qasm.GateCRZ param ops mod1))
    where ops   = [decl2at 3, decl2at 2]
          param = DecFloat "2.0"

-----------------------------------------------------------------------------------------
-- Translating controlled CRZ gates (many configuration).

test45 = TestCase (assertEqual "Translation of a controlled CRZ gate (1/2)."
                               [RotGate Quip.RotExpZ False 0.5 [0] ctrls]
                               (d1RotTransl alloc4 Qasm.GateCRZ param ops mod2))
    where ops   = [decl2at 1, decl2at 2, decl2at 0, decl1]
          ctrls = [Pos 1, Pos 2, Pos 3]
          param = DecInt "1"

test46 = TestCase (assertEqual "Translation of a CRZ gate (2/2)."
                               [RotGate Quip.RotExpZ True 1.0 [2] ctrls]
                               (d1RotTransl alloc4 Qasm.GateCRZ param ops mod5))
    where ops   = [decl2at 3, decl2at 4, decl2at 2, decl2at 1]
          ctrls = [Pos 3, Pos 4, Neg 5]
          param = DecFloat "2.0"

-----------------------------------------------------------------------------------------
-- Translating U1 and Phase gates (many configuration).

toPhaseTest1 :: String -> Qasm.GateName -> Test.HUnit.Test
toPhaseTest1 msg gate =
    case toConstFloat $ Div (DecFloat "1.0") Pi of
        Right err  -> TestCase (assertFailure "Unable to parse phase.")
        Left phase -> let pgate = PhaseGate phase [] 
                      in TestCase (assertEqual msg [pgate, rgate] act)
    where param = DecInt "2"
          rgate = RotGate Quip.RotExpZ False 1.0 [0] []
          act   = d1RotTransl alloc4 gate param [decl1] mod0

toPhaseTest2 :: String -> Qasm.GateName -> Test.HUnit.Test
toPhaseTest2 msg gate =
    case toConstFloat $ Div (DecFloat "2.0") Pi of
        Right err  -> TestCase (assertFailure "Unable to parse phase.")
        Left phase -> let pgate = PhaseGate (-phase) [] 
                      in TestCase (assertEqual msg [pgate, rgate] act)
    where param = DecFloat "4.0"
          rgate = RotGate Quip.RotExpZ True 2.0 [2] []
          act   = d1RotTransl alloc4 gate param [decl2at 1] mod1

test47 = toPhaseTest1 "Translation of an U1 gate (1/2)." Qasm.GateU1
test48 = toPhaseTest2 "Translation of an U1 gate (2/2)." Qasm.GateU1

test49 = toPhaseTest1 "Translation of an Phase gate (1/2)." Qasm.GatePhase
test50 = toPhaseTest2 "Translation of an Phase gate (2/2)." Qasm.GatePhase

-----------------------------------------------------------------------------------------
-- Translating GPhase gates (many configuration).

test51 = case toConstFloat $ Div (DecFloat "1.0") Pi of
    Right err  -> TestCase (assertFailure "Unable to parse phase.")
    Left phase -> let pgate = PhaseGate phase ctrls
                  in TestCase (assertEqual msg [pgate, rgate] act)
    where msg   = "Translation of a CPhase gate (1/2)."
          param = DecInt "2"
          decls = [decl2at 0, decl1]
          ctrls = [Pos 1]
          rgate = RotGate Quip.RotExpZ False 1.0 [0] ctrls
          act   = d1RotTransl alloc4 Qasm.GateCPhase param decls mod0

test52 = case toConstFloat $ Div (DecFloat "2.0") Pi of
    Right err  -> TestCase (assertFailure "Unable to parse phase.")
    Left phase -> let pgate = PhaseGate (-phase) ctrls
                  in TestCase (assertEqual msg [pgate, rgate] act)
    where msg   = "Translation of a CPhase gate (2/2)."
          param = DecFloat "4.0"
          decls = [decl2at 2, decl2at 1]
          ctrls = [Pos 3]
          rgate = RotGate Quip.RotExpZ True 2.0 [2] ctrls
          act   = d1RotTransl alloc4 Qasm.GateCPhase param decls mod1

-----------------------------------------------------------------------------------------
-- Translating U1 and Phase gates with controls (many configuration).

toPhaseTest3 :: String -> Qasm.GateName -> Test.HUnit.Test
toPhaseTest3 msg gate =
    case toConstFloat $ Div (DecFloat "1.0") Pi of
        Right err  -> TestCase (assertFailure "Unable to parse phase.")
        Left phase -> let pgate = PhaseGate phase ctrls
                      in TestCase (assertEqual msg [pgate, rgate] act)
    where param = DecInt "2"
          decls = [decl2at 1, decl2at 0, decl1]
          ctrls = [Pos 2, Pos 1]
          rgate = RotGate Quip.RotExpZ False 1.0 [0] ctrls
          act   = d1RotTransl alloc4 gate param decls mod2

toPhaseTest4 :: String -> Qasm.GateName -> Test.HUnit.Test
toPhaseTest4 msg gate =
    case toConstFloat $ Div (DecFloat "2.0") Pi of
        Right err  -> TestCase (assertFailure "Unable to parse phase.")
        Left phase -> let pgate = PhaseGate (-phase) ctrls
                      in TestCase (assertEqual msg [pgate, rgate] act)
    where param = DecFloat "4.0"
          decls = [decl2at 3, decl2at 4, decl2at 2]
          ctrls = [Pos 4, Neg 5]
          rgate = RotGate Quip.RotExpZ True 2.0 [3] ctrls
          act   = d1RotTransl alloc4 gate param decls mod5

test53 = toPhaseTest3 "Translation of a controlled U1 gate (1/2)." Qasm.GateU1
test54 = toPhaseTest4 "Translation of a controlled U1 gate (2/2)." Qasm.GateU1

test55 = toPhaseTest3 "Translation of a controlled Phase gate (1/2)." Qasm.GatePhase
test56 = toPhaseTest4 "Translation of a controlled Phase gate (2/2)." Qasm.GatePhase

-----------------------------------------------------------------------------------------
-- Translating controlled GPhase gates (many configuration).

test57 = case toConstFloat $ Div (DecFloat "1.0") Pi of
    Right err  -> TestCase (assertFailure "Unable to parse phase.")
    Left phase -> let pgate = PhaseGate phase ctrls
                  in TestCase (assertEqual msg [pgate, rgate] act)
    where msg   = "Translation of a controlled CPhase gate (2/2)."
          param = DecInt "2"
          decls = [decl2at 1, decl2at 2, decl2at 0, decl1]
          ctrls = [Pos 1, Pos 2, Pos 3]
          rgate = RotGate Quip.RotExpZ False 1.0 [0] ctrls
          act   = d1RotTransl alloc4 Qasm.GateCPhase param decls mod2

test58 = case toConstFloat $ Div (DecFloat "2.0") Pi of
    Right err  -> TestCase (assertFailure "Unable to parse phase.")
    Left phase -> let pgate = PhaseGate (-phase) ctrls
                  in TestCase (assertEqual msg [pgate, rgate] act)
    where msg   = "Translation of a controlled CPhase gate (2/2)."
          param = DecFloat "4.0"
          decls = [decl2at 3, decl2at 4, decl2at 2, decl2at 1]
          ctrls = [Pos 3, Pos 4, Neg 5]
          rgate = RotGate Quip.RotExpZ True 2.0 [2] ctrls
          act   = d1RotTransl alloc4 Qasm.GateCPhase param decls mod5

-----------------------------------------------------------------------------------------
-- Translating P gates (many configuration).

-- Remark: GateP should route to GPhase translation, so the same test cases are used.

test59 = TestCase (assertEqual "Translation of P gates without controls."
                               [PhaseGate 5.0 [Pos 0]]
                               (d1RotTransl alloc4 Qasm.GateP param [decl1] mod0))
    where param = Times Pi $ DecFloat "5.0"

test60 = TestCase (assertEqual "Translation of P gates with inversion."
                               [PhaseGate (-7.1) [Pos 1]]
                               (d1RotTransl alloc4 Qasm.GateP param [decl2at 0] mod1))
    where param = Times Pi $ DecFloat "7.1"

test61 = TestCase (assertEqual "Translation of P gates with controls."
                               [PhaseGate 0 ctrls]
                               (d1RotTransl alloc4 Qasm.GateP param ops mod2))
    where ops   = [decl1, decl2at 2, decl2at 1]
          ctrls = [Pos 2, Pos 0, Pos 3]
          param = Times Pi $ DecFloat "0"

-----------------------------------------------------------------------------------------
-- Translating CP gates (many configuration).

test62 = TestCase (assertEqual "Translation of CP gates without controls."
                               [PhaseGate 5.0 ctrls]
                               (d1RotTransl alloc4 Qasm.GateCP param ops mod0))
    where ops   = [decl2at 0, decl1]
          ctrls = [Pos 1, Pos 0]
          param = Times Pi $ DecFloat "5.0"

test63 = TestCase (assertEqual "Translation of CP gates with inversion."
                               [PhaseGate (-7.1) ctrls]
                               (d1RotTransl alloc4 Qasm.GateCP param ops mod1))
    where ops   = [decl2at 2, decl2at 1]
          ctrls = [Pos 3, Pos 2]
          param = Times Pi $ DecFloat "7.1"

test64 = TestCase (assertEqual "Translation of CP gates with controls."
                               [PhaseGate 0 ctrls]
                               (d1RotTransl alloc4 Qasm.GateCP param ops mod2))
    where ops   = [decl1, decl2at 2, decl2at 1, decl2at 3]
          ctrls = [Pos 2, Pos 4, Pos 0, Pos 3]
          param = Times Pi $ DecFloat "0"

-----------------------------------------------------------------------------------------
-- Translating U2 gates (single controlled, single uncontrolled, as tedious to test).

test65 = case toConstFloat $ Div a1 $ DecInt "2" of
    Right err  -> TestCase (assertFailure "Unable to parse p1.")
    Left t1    -> case toConstFloat $ Div a2 $ DecInt "2" of
        Right err  -> TestCase (assertFailure "Unable to parse p1.")
        Left t2    -> let circ = [NamedGate Quip.GateOmega False    [0] [],
                                  RotGate   Quip.RotExpZ   False t1 [0] [],
                                  NamedGate Quip.GateH     False    [0] [],
                                  NamedGate Quip.GateS     False    [0] [],
                                  NamedGate Quip.GateH     False    [0] [],
                                  RotGate   Quip.RotExpZ   False t2 [0] []]
                      in TestCase (assertEqual msg circ expt)
    where msg  = "Translation of U2 gates without controls."
          p1   = Times Pi $ DecFloat "0.25"
          p2   = Times Pi $ DecFloat "0.33"
          a1   = Plus p1 $ Div Pi $ DecInt "2"
          a2   = Minus p2 $ Div Pi $ DecInt "2"
          expt = d2RotTransl alloc4 Qasm.GateU2 (p1, p2) [decl1] mod0

test66 = case toConstFloat $ Div a1 $ DecInt "2" of
    Right err  -> TestCase (assertFailure "Unable to parse p1.")
    Left t1    -> case toConstFloat $ Div a2 $ DecInt "2" of
        Right err  -> TestCase (assertFailure "Unable to parse p1.")
        Left t2    -> let circ = [NamedGate Quip.GateOmega True     [2] ctrls,
                                  RotGate   Quip.RotExpZ   False t1 [2] ctrls,
                                  NamedGate Quip.GateH     False    [2] [],
                                  NamedGate Quip.GateS     True     [2] ctrls,
                                  NamedGate Quip.GateH     False    [2] [],
                                  RotGate   Quip.RotExpZ   False t2 [2] ctrls]
                      in TestCase (assertEqual msg circ expt)
    where msg   = "Translation of U2 gates with controls."
          p1    = Times Pi $ DecFloat "0.27"
          p2    = Times Pi $ DecFloat "0.36"
          a1    = Plus  (Negate p2) $ Div Pi $ DecInt "2"
          a2    = Minus (Negate p1) $ Div Pi $ DecInt "2"
          ops   = [decl2at 3, decl2at 2, decl2at 1]
          ctrls = [Pos 4, Pos 3]
          expt  = d2RotTransl alloc4 Qasm.GateU2 (p1, p2) ops mod4

-----------------------------------------------------------------------------------------
-- Translating U3 gates (single controlled, single uncontrolled, as tedious to test).

test67 = case toConstFloat $ Div p1 Pi of
    Right err -> TestCase (assertFailure "Unable to parse p1.")
    Left ph   -> case toConstFloat $ Div p1 $ DecInt "2" of
        Right err -> TestCase (assertFailure "Unable to parse p2.")
        Left t1   -> case toConstFloat $ Div a1 $ DecInt "2" of
            Right err -> TestCase (assertFailure "Unable to parse p1.")
            Left t2   -> case toConstFloat $ Div a2 $ DecInt "2" of
                Right err -> TestCase (assertFailure "Unable to parse p3.")
                Left t3   -> let circ = [NamedGate Quip.GateOmega False    [0] [],
                                         NamedGate Quip.GateOmega False    [0] [],
                                         PhaseGate                      ph     [],
                                         RotGate   Quip.RotExpZ   False t1 [0] [],
                                         NamedGate Quip.GateH     False    [0] [],
                                         RotGate   Quip.RotExpZ   False t2 [0] [],
                                         NamedGate Quip.GateH     False    [0] [],
                                         RotGate   Quip.RotExpZ   False t3 [0] []]
                             in TestCase (assertEqual msg circ expt)
    where msg  = "Translation of U3 gates without controls."
          p1   = Times Pi $ DecFloat "0.25"
          p2   = Times Pi $ DecFloat "0.33"
          p3   = Times Pi $ DecFloat "0.5"
          a1   = Plus p2 $ Div Pi $ DecInt "2"
          a2   = Minus p3 $ Div Pi $ DecInt "2"
          expt = d3RotTransl alloc4 Qasm.GateU3 (p1, p2, p3) [decl1] mod0

test68 = case toConstFloat $ Negate $ Div p1 Pi of
    Right err -> TestCase (assertFailure "Unable to parse p1.")
    Left ph   -> case toConstFloat $ Negate $ Div p1 $ DecInt "2" of
        Right err -> TestCase (assertFailure "Unable to parse p2.")
        Left t1   -> case toConstFloat $ Div (Negate a2) $ DecInt "2" of
            Right err -> TestCase (assertFailure "Unable to parse p1.")
            Left t2   -> case toConstFloat $ Div (Negate a1) $ DecInt "2" of
                Right err -> TestCase (assertFailure "Unable to parse p3.")
                Left t3   -> let circ = [NamedGate Quip.GateOmega True     [2] ctrls,
                                         NamedGate Quip.GateOmega True     [2] ctrls,
                                         PhaseGate                      ph     ctrls,
                                         RotGate   Quip.RotExpZ   False t1 [2] ctrls,
                                         NamedGate Quip.GateH     False    [2] [],
                                         RotGate   Quip.RotExpZ   False t2 [2] ctrls,
                                         NamedGate Quip.GateH     False    [2] [],
                                         RotGate   Quip.RotExpZ   False t3 [2] ctrls]
                             in TestCase (assertEqual msg circ expt)
    where msg   = "Translation of U3 gates with controls."
          p1    = Times Pi $ DecFloat "0.27"
          p2    = Times Pi $ DecFloat "0.36"
          p3    = Times Pi $ DecFloat "0.54"
          a1    = Plus p2 $ Div Pi $ DecInt "2"
          a2    = Minus p3 $ Div Pi $ DecInt "2"
          ops   = [decl2at 3, decl2at 2, decl2at 1]
          ctrls = [Pos 4, Pos 3]
          expt  = d3RotTransl alloc4 Qasm.GateU3 (p1, p2, p3) ops mod4

-----------------------------------------------------------------------------------------
-- Translating U gates (single controlled, single uncontrolled, as tedious to test).

test69 = case toConstFloat $ Div p1 Pi of
    Right err -> TestCase (assertFailure "Unable to parse p1.")
    Left ph   -> case toConstFloat $ Div p1 $ DecInt "2" of
        Right err -> TestCase (assertFailure "Unable to parse p2.")
        Left t1   -> case toConstFloat $ Div a1 $ DecInt "2" of
            Right err -> TestCase (assertFailure "Unable to parse p1.")
            Left t2   -> case toConstFloat $ Div a2 $ DecInt "2" of
                Right err -> TestCase (assertFailure "Unable to parse p3.")
                Left t3   -> case toConstFloat $ Div (Plus p2 p3) pi2 of
                    Right err -> TestCase (assertFailure "Unable to parse p2+p3/2.")
                    Left  ut  -> let circ = [PhaseGate                      ut     [],
                                             NamedGate Quip.GateOmega False    [0] [],
                                             NamedGate Quip.GateOmega False    [0] [],
                                             PhaseGate                      ph     [],
                                             RotGate   Quip.RotExpZ   False t1 [0] [],
                                             NamedGate Quip.GateH     False    [0] [],
                                             RotGate   Quip.RotExpZ   False t2 [0] [],
                                             NamedGate Quip.GateH     False    [0] [],
                                             RotGate   Quip.RotExpZ   False t3 [0] []]
                                 in TestCase (assertEqual msg circ expt)
    where msg  = "Translation of U gates without controls."
          pi2  = Times Pi $ DecInt "2"
          p1   = Times Pi $ DecFloat "0.25"
          p2   = Times Pi $ DecFloat "0.33"
          p3   = Times Pi $ DecFloat "0.5"
          a1   = Plus p2 $ Div Pi $ DecInt "2"
          a2   = Minus p3 $ Div Pi $ DecInt "2"
          expt = d3RotTransl alloc4 Qasm.GateU (p1, p2, p3) [decl1] mod0

test70 = case toConstFloat $ Negate $ Div p1 Pi of
    Right err -> TestCase (assertFailure "Unable to parse p1.")
    Left ph   -> case toConstFloat $ Negate $ Div p1 $ DecInt "2" of
        Right err -> TestCase (assertFailure "Unable to parse p2.")
        Left t1   -> case toConstFloat $ Div (Negate a2) $ DecInt "2" of
            Right err -> TestCase (assertFailure "Unable to parse p1.")
            Left t2   -> case toConstFloat $ Div (Negate a1) $ DecInt "2" of
                Right err -> TestCase (assertFailure "Unable to parse p3.")    
                Left t3   -> case toConstFloat $ Div (Negate $ Plus p2 p3) pi2 of
                    Right err -> TestCase (assertFailure "Unable to parse p2+p3/2.")
                    Left ut   -> let circ = [PhaseGate                      ut     ctrls,
                                             NamedGate Quip.GateOmega True     [2] ctrls,
                                             NamedGate Quip.GateOmega True     [2] ctrls,
                                             PhaseGate                      ph     ctrls,
                                             RotGate   Quip.RotExpZ   False t1 [2] ctrls,
                                             NamedGate Quip.GateH     False    [2] [],
                                             RotGate   Quip.RotExpZ   False t2 [2] ctrls,
                                             NamedGate Quip.GateH     False    [2] [],
                                             RotGate   Quip.RotExpZ   False t3 [2] ctrls]
                                 in TestCase (assertEqual msg circ expt)
    where msg   = "Translation of U gates with controls."
          pi2   = Times Pi $ DecInt "2"
          p1    = Times Pi $ DecFloat "0.27"
          p2    = Times Pi $ DecFloat "0.36"
          p3    = Times Pi $ DecFloat "0.54"
          a1    = Plus p2 $ Div Pi $ DecInt "2"
          a2    = Minus p3 $ Div Pi $ DecInt "2"
          ops   = [decl2at 3, decl2at 2, decl2at 1]
          ctrls = [Pos 4, Pos 3]
          expt  = d3RotTransl alloc4 Qasm.GateU (p1, p2, p3) ops mod4

-----------------------------------------------------------------------------------------
-- Translating CU gates (single controlled, single uncontrolled, as tedious to test).

test71 = case toConstFloat $ Div p1 Pi of
    Right err -> TestCase (assertFailure "Unable to parse p1.")
    Left ph   -> case toConstFloat $ Div p1 $ DecInt "2" of
        Right err -> TestCase (assertFailure "Unable to parse p2.")
        Left t1   -> case toConstFloat $ Div a1 $ DecInt "2" of
            Right err -> TestCase (assertFailure "Unable to parse p1.")
            Left t2   -> case toConstFloat $ Div a2 $ DecInt "2" of
                Right err -> TestCase (assertFailure "Unable to parse p3.")
                Left t3   -> case toConstFloat $ Div (Plus p2 p3) pi2 of
                    Right err -> TestCase (assertFailure "Unable to parse p2+p3/2.")
                    Left  ut  -> case toConstFloat $ Div p4 Pi of
                        Right err -> TestCase (assertFailure "Unable to parse p4.")
                        Left t4   -> let circ = [PhaseGate                      t4     ctrls,
                                                 PhaseGate                      ut     ctrls,
                                                 NamedGate Quip.GateOmega False    [0] ctrls,
                                                 NamedGate Quip.GateOmega False    [0] ctrls,
                                                 PhaseGate                      ph     ctrls,
                                                 RotGate   Quip.RotExpZ   False t1 [0] ctrls,
                                                 NamedGate Quip.GateH     False    [0] [],
                                                 RotGate   Quip.RotExpZ   False t2 [0] ctrls,
                                                 NamedGate Quip.GateH     False    [0] [],
                                                 RotGate   Quip.RotExpZ   False t3 [0] ctrls]
                                 in TestCase (assertEqual msg circ expt)
    where msg   = "Translation of CU gates without explicit controls."
          pi2   = Times Pi $ DecInt "2"
          p1    = Times Pi $ DecFloat "0.25"
          p2    = Times Pi $ DecFloat "0.33"
          p3    = Times Pi $ DecFloat "0.5"
          p4    = Times Pi $ DecFloat "0.1"
          a1    = Plus p2 $ Div Pi $ DecInt "2"
          a2    = Minus p3 $ Div Pi $ DecInt "2"
          ops   = [decl2at 0, decl1]
          ctrls = [Pos 1]
          expt  = d4RotTransl alloc4 Qasm.GateCU (p1, p2, p3, p4) ops mod0

test72 = case toConstFloat $ Negate $ Div p1 Pi of
    Right err -> TestCase (assertFailure "Unable to parse p1.")
    Left ph   -> case toConstFloat $ Negate $ Div p1 $ DecInt "2" of
        Right err -> TestCase (assertFailure "Unable to parse p2.")
        Left t1   -> case toConstFloat $ Div (Negate a2) $ DecInt "2" of
            Right err -> TestCase (assertFailure "Unable to parse p1.")
            Left t2   -> case toConstFloat $ Div (Negate a1) $ DecInt "2" of
                Right err -> TestCase (assertFailure "Unable to parse p3.")    
                Left t3   -> case toConstFloat $ Div (Negate $ Plus p2 p3) pi2 of
                    Right err -> TestCase (assertFailure "Unable to parse p2+p3/2.")
                    Left ut   -> case toConstFloat $ Div (Negate p4) Pi of
                        Right err -> TestCase (assertFailure "Unable to parse p4.")
                        Left t4   -> let circ = [PhaseGate                      t4     ctrls,
                                                 PhaseGate                      ut     ctrls,
                                                 NamedGate Quip.GateOmega True     [2] ctrls,
                                                 NamedGate Quip.GateOmega True     [2] ctrls,
                                                 PhaseGate                      ph     ctrls,
                                                 RotGate   Quip.RotExpZ   False t1 [2] ctrls,
                                                 NamedGate Quip.GateH     False    [2] [],
                                                 RotGate   Quip.RotExpZ   False t2 [2] ctrls,
                                                 NamedGate Quip.GateH     False    [2] [],
                                                 RotGate   Quip.RotExpZ   False t3 [2] ctrls]
                                 in TestCase (assertEqual msg circ expt)
    where msg   = "Translation of U gates with controls."
          pi2   = Times Pi $ DecInt "2"
          p1    = Times Pi $ DecFloat "0.27"
          p2    = Times Pi $ DecFloat "0.36"
          p3    = Times Pi $ DecFloat "0.54"
          p4    = Times Pi $ DecFloat "0.91"
          a1    = Plus p2 $ Div Pi $ DecInt "2"
          a2    = Minus p3 $ Div Pi $ DecInt "2"
          ops   = [decl2at 3, decl2at 2, decl2at 4, decl2at 1]
          ctrls = [Pos 5, Pos 4, Pos 3]
          expt  = d4RotTransl alloc4 Qasm.GateCU (p1, p2, p3, p4) ops mod4

-----------------------------------------------------------------------------------------
-- Translating RX gates (single controlled, single uncontrolled, as tedious to test).

test73 = case toConstFloat $ Div param $ Times Pi $ DecInt "2" of
    Right err -> TestCase (assertFailure "Unable to parse p1.")
    Left pt   -> let circ = [PhaseGate                    pt      [],
                             NamedGate Quip.GateH   False     [0] [],
                             RotGate   Quip.RotExpZ False 0.5 [0] [],
                             NamedGate Quip.GateH   False     [0] []]
                 in TestCase (assertEqual msg circ expt)
    where msg   = "Translation of an RX gate (1/2)."
          param = DecInt "1"
          expt  = d1RotTransl alloc4 Qasm.GateRX param [decl1] mod0

test74 = case toConstFloat $ Negate $ Div param $ Times Pi $ DecInt "2" of
    Right err -> TestCase (assertFailure "Unable to parse p1.")
    Left pt   -> let circ = [PhaseGate                    pt      ctrls,
                             NamedGate Quip.GateH   False     [2] [],
                             RotGate   Quip.RotExpZ True  1.0 [2] ctrls,
                             NamedGate Quip.GateH   False     [2] []]
                 in TestCase (assertEqual msg circ expt)
    where msg   = "Translation of an RX gate (2/2)."
          param = DecFloat "2.0"
          ops   = [decl2at 3, decl2at 2, decl2at 1]
          ctrls = [Pos 4, Pos 3]
          expt  = d1RotTransl alloc4 Qasm.GateRX param ops mod4

-----------------------------------------------------------------------------------------
-- Translating CRX gates (single controlled, single uncontrolled, as tedious to test).

test75 = case toConstFloat $ Div param $ Times Pi $ DecInt "2" of
    Right err -> TestCase (assertFailure "Unable to parse p1.")
    Left pt   -> let circ = [PhaseGate                    pt      ctrls,
                             NamedGate Quip.GateH   False     [0] [],
                             RotGate   Quip.RotExpZ False 0.5 [0] ctrls,
                             NamedGate Quip.GateH   False     [0] []]
                 in TestCase (assertEqual msg circ expt)
    where msg   = "Translation of an CRX gate (1/2)."
          param = DecInt "1"
          ops   = [decl2at 0, decl1]
          ctrls = [Pos 1]
          expt  = d1RotTransl alloc4 Qasm.GateCRX param ops mod0

test76 = case toConstFloat $ Negate $ Div param $ Times Pi $ DecInt "2" of
    Right err -> TestCase (assertFailure "Unable to parse p1.")
    Left pt   -> let circ = [PhaseGate                    pt      ctrls,
                             NamedGate Quip.GateH   False     [2] [],
                             RotGate   Quip.RotExpZ True  1.0 [2] ctrls,
                             NamedGate Quip.GateH   False     [2] []]
                 in TestCase (assertEqual msg circ expt)
    where msg   = "Translation of an CRX gate (2/2)."
          param = DecFloat "2.0"
          ops   = [decl2at 3, decl2at 2, decl2at 0, decl2at 1]
          ctrls = [Pos 1, Pos 4, Pos 3]
          expt  = d1RotTransl alloc4 Qasm.GateCRX param ops mod4

-----------------------------------------------------------------------------------------
-- Translating RY gates (single controlled, single uncontrolled, as tedious to test).

test77 = case toConstFloat $ Div param Pi of
    Right err -> TestCase (assertFailure "Unable to parse p1.")
    Left pt   -> let circ = [PhaseGate                    pt      [],
                             NamedGate Quip.GateS   False     [0] [],
                             NamedGate Quip.GateH   False     [0] [],
                             RotGate   Quip.RotExpZ False 0.5 [0] [],
                             NamedGate Quip.GateH   False     [0] [],
                             NamedGate Quip.GateS   True      [0] []]
                 in TestCase (assertEqual msg circ expt)
    where msg   = "Translation of an RY gate (1/2)."
          param = DecInt "1"
          expt  = d1RotTransl alloc4 Qasm.GateRY param [decl1] mod0

test78 = case toConstFloat $ Negate $ Div param Pi of
    Right err -> TestCase (assertFailure "Unable to parse p1.")
    Left pt   -> let circ = [PhaseGate                    pt      ctrls,
                             NamedGate Quip.GateS   False     [2] [],
                             NamedGate Quip.GateH   False     [2] [],
                             RotGate   Quip.RotExpZ True  1.0 [2] ctrls,
                             NamedGate Quip.GateH   False     [2] [],
                             NamedGate Quip.GateS   True      [2] []]
                 in TestCase (assertEqual msg circ expt)
    where msg   = "Translation of an RY gate (2/2)."
          param = DecFloat "2.0"
          ops   = [decl2at 3, decl2at 2, decl2at 1]
          ctrls = [Pos 4, Pos 3]
          expt  = d1RotTransl alloc4 Qasm.GateRY param ops mod4

-----------------------------------------------------------------------------------------
-- Translating CRY gates (single controlled, single uncontrolled, as tedious to test).

test79 = case toConstFloat $ Div param Pi of
    Right err -> TestCase (assertFailure "Unable to parse p1.")
    Left pt   -> let circ = [PhaseGate                    pt      ctrls,
                             NamedGate Quip.GateS   False     [0] [],
                             NamedGate Quip.GateH   False     [0] [],
                             RotGate   Quip.RotExpZ False 0.5 [0] ctrls,
                             NamedGate Quip.GateH   False     [0] [],
                             NamedGate Quip.GateS   True      [0] []]
                 in TestCase (assertEqual msg circ expt)
    where msg   = "Translation of an CRY gate (1/2)."
          param = DecInt "1"
          ops   = [decl2at 0, decl1]
          ctrls = [Pos 1]
          expt  = d1RotTransl alloc4 Qasm.GateCRY param ops mod0

test80 = case toConstFloat $ Negate $ Div param Pi of
    Right err -> TestCase (assertFailure "Unable to parse p1.")
    Left pt   -> let circ = [PhaseGate                    pt      ctrls,
                             NamedGate Quip.GateS   False     [2] [],
                             NamedGate Quip.GateH   False     [2] [],
                             RotGate   Quip.RotExpZ True  1.0 [2] ctrls,
                             NamedGate Quip.GateH   False     [2] [],
                             NamedGate Quip.GateS   True      [2] []]
                 in TestCase (assertEqual msg circ expt)
    where msg   = "Translation of an CRY gate (2/2)."
          param = DecFloat "2.0"
          ops   = [decl2at 3, decl2at 2, decl2at 0, decl2at 1]
          ctrls = [Pos 1, Pos 4, Pos 3]
          expt  = d1RotTransl alloc4 Qasm.GateCRY param ops mod4

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "GateS_1" test1,
                                     TestLabel "GateS_2" test2,
                                     TestLabel "GateS_3" test3,
                                     TestLabel "GateS_4" test4,
                                     TestLabel "GateS_5" test5,
                                     TestLabel "GateS_6" test6,
                                     TestLabel "GateX" test7,
                                     TestLabel "GateY" test8,
                                     TestLabel "GateZ" test9,
                                     TestLabel "GateH" test10,
                                     TestLabel "GateSwap" test11,
                                     TestLabel "GateSX" test12,
                                     TestLabel "GateT" test13,
                                     TestLabel "GateIX" test14,
                                     TestLabel "GateOmega" test15,
                                     TestLabel "GateE" test16,
                                     TestLabel "GateW" test17,
                                     TestLabel "GateSdg_1" test18,
                                     TestLabel "GateSdg_2" test19,
                                     TestLabel "GateSdg_3" test20,
                                     TestLabel "GateSdg_4" test21,
                                     TestLabel "GateSdg_5" test22,
                                     TestLabel "GateSdg_6" test23,
                                     TestLabel "GateTdg" test24,
                                     TestLabel "GateCX_1" test25,
                                     TestLabel "GateCX_2" test26,
                                     TestLabel "GateCX_3" test27,
                                     TestLabel "GateCX_4" test28,
                                     TestLabel "GateCX_5" test29,
                                     TestLabel "GateCX_6" test30,
                                     TestLabel "GateCY" test31,
                                     TestLabel "GateCZ" test32,
                                     TestLabel "GateCH" test33,
                                     TestLabel "GateCSwap" test34,
                                     TestLabel "GateToffoli" test35,
                                     TestLabel "GPhase_1" test36,
                                     TestLabel "GPhase_2" test37,
                                     TestLabel "GPhase_3" test38,
                                     TestLabel "RZ_0Ctrl_1" test39,
                                     TestLabel "RZ_0Ctrl_2" test40,
                                     TestLabel "RZ_2Ctrl_2" test41,
                                     TestLabel "RZ_2Ctrl_2" test42,
                                     TestLabel "CRZ_0Ctrl_1" test43,
                                     TestLabel "CRZ_0Ctrl_2" test44,
                                     TestLabel "CRZ_2Ctrl_1" test45,
                                     TestLabel "CRZ_2Ctrl_2" test46,
                                     TestLabel "U1_0Ctrl_1" test47,
                                     TestLabel "U1_0Ctrl_2" test48,
                                     TestLabel "Phase_0Ctrl_1" test49,
                                     TestLabel "Phase_0Ctrl_2" test50,
                                     TestLabel "CPhase_0Ctrl_1" test51,
                                     TestLabel "CPhase_0Ctrl_2" test52,
                                     TestLabel "U1_2Ctrl_1" test53,
                                     TestLabel "U1_2Ctrl_2" test54,
                                     TestLabel "Phase_2Ctrl_1" test55,
                                     TestLabel "Phase_2Ctrl_2" test56,
                                     TestLabel "CPhase_2Ctrl_1" test57,
                                     TestLabel "CPhase_2Ctrl_2" test58,
                                     TestLabel "GateP_1" test59,
                                     TestLabel "GateP_2" test60,
                                     TestLabel "GateP_3" test61,
                                     TestLabel "GateCP_1" test62,
                                     TestLabel "GateCP_2" test63,
                                     TestLabel "GateCP_3" test64,
                                     TestLabel "GateU2_1" test65,
                                     TestLabel "GateU2_2" test66,
                                     TestLabel "GateU3_1" test67,
                                     TestLabel "GateU3_2" test68,
                                     TestLabel "GateU_1" test69,
                                     TestLabel "GateU_2" test70,
                                     TestLabel "GateCU_1" test71,
                                     TestLabel "GateCU_2" test72,
                                     TestLabel "GateRX_1" test73,
                                     TestLabel "GateRX_2" test74,
                                     TestLabel "GateCRX_1" test75,
                                     TestLabel "GateCRX_2" test76,
                                     TestLabel "GateRY_1" test77,
                                     TestLabel "GateRY_2" test78,
                                     TestLabel "GateCRY_1" test79,
                                     TestLabel "GateCRY_2" test80]

main = defaultMain tests
