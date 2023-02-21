-- | Functions to translate assignment statements from OpenQASM to Quipper.

module LinguaQuanta.QasmToQuip.Assign
  ( translateCDiscard
  , translateCInit0
  , translateCInit1
  , translateCTerm0
  , translateCTerm1
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.QasmToQuip.Operand (toOperand)
import LinguaQuanta.QasmToQuip.Ancilla
  ( translateAncilla
  , updateWireMap
  )
import LinguaQuanta.QasmToQuip.Wire
  ( WireAllocMap
  , initCell
  , initScalar
  , termCell
  , termScalar
  )
import LinguaQuanta.Quip.Gate (Gate(..))

-------------------------------------------------------------------------------
-- * Ancilla Translation.

-- | Represents the gates produced by a translation, and the resulting changes
-- to the WireAllocMap.
type TranslRes = (WireAllocMap, [Gate])

-- | Takes as input a wire allocation map and the lhs of an assignment
-- (represented by a String for the declaration name, and a Maybe Int for the
-- optional array cell index). Assumes that the rhs of the assignment is a void
-- call to CDiscard, and returns the corresponding Quipper gates, together with
-- the new allocation map (the wire is terminated).
translateCDiscard :: WireAllocMap -> String -> Maybe Int -> TranslRes
translateCDiscard wmap decl idx = (wmap', gates)
    where argop = toOperand decl idx
          gates = translateAncilla wmap argop CDiscardGate
          wmap' = updateWireMap wmap argop (termScalar, termCell)

-- | Takes as input a wire allocation map and the lhs of an assignment
-- (represented by a String for the declaration name, and a Maybe Int for the
-- optional array cell index). Assumes that the rhs of the assignment is a void
-- call to CInit0, and returns the corresponding Quipper gates, together with
-- the new allocation map (the wire is initialized).
translateCInit0 :: WireAllocMap -> String -> Maybe Int -> TranslRes
translateCInit0 wmap decl idx = (wmap', gates)
    where argop = toOperand decl idx
          gates = translateAncilla wmap argop $ CInitGate False
          wmap' = updateWireMap wmap argop (initScalar, initCell)

-- | Takes as input a wire allocation map and the lhs of an assignment
-- (represented by a String for the declaration name, and a Maybe Int for the
-- optional array cell index). Assumes that the rhs of the assignment is a void
-- call to CInit1, and returns the corresponding Quipper gates, together with
-- the new allocation map (the wire is initialized).
translateCInit1 :: WireAllocMap -> String -> Maybe Int -> TranslRes
translateCInit1 wmap decl idx = (wmap', gates)
    where argop = toOperand decl idx
          gates = translateAncilla wmap argop $ CInitGate True
          wmap' = updateWireMap wmap argop (initScalar, initCell)

-- | Takes as input a wire allocation map and the lhs of an assignment
-- (represented by a String for the declaration name, and a Maybe Int for the
-- optional array cell index). Assumes that the rhs of the assignment is a void
-- call to CTerm0, and returns the corresponding Quipper gates, together with
-- the new allocation map (the wire is terminated).
translateCTerm0 :: WireAllocMap -> String -> Maybe Int -> TranslRes
translateCTerm0 wmap decl idx = (wmap', gates)
    where argop = toOperand decl idx
          gates = translateAncilla wmap argop $ CTermGate False
          wmap' = updateWireMap wmap argop (termScalar, termCell)

-- | Takes as input a wire allocation map and the lhs of an assignment
-- (represented by a String for the declaration name, and a Maybe Int for the
-- optional array cell index). Assumes that the rhs of the assignment is a void
-- call to CTerm1, and returns the corresponding Quipper gates, together with
-- the new allocation map (the wire is terminated).
translateCTerm1 :: WireAllocMap -> String -> Maybe Int -> TranslRes
translateCTerm1 wmap decl idx = (wmap', gates)
    where argop = toOperand decl idx
          gates = translateAncilla wmap argop $ CTermGate True
          wmap' = updateWireMap wmap argop (termScalar, termCell)
