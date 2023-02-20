-- | Functions to translate call statements from OpenQASM to Quipper.

module LinguaQuanta.QasmToQuip.Call
  ( translateQDiscard
  , translateQInit0
  , translateQInit1
  , translateQTerm0
  , translateQTerm1
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.Operand (Operand)
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
-- call to QDiscard, and returns the corresponding Quipper gates, together with
-- the new allocation map (the wire is terminated).
translateQDiscard :: WireAllocMap -> Operand -> TranslRes
translateQDiscard wmap op = (wmap', gates)
    where gates = translateAncilla wmap op QDiscardGate
          wmap' = updateWireMap wmap op (termScalar, termCell)

-- | Takes as input a wire allocation map and the lhs of an assignment
-- (represented by a String for the declaration name, and a Maybe Int for the
-- optional array cell index). Assumes that the rhs of the assignment is a void
-- call to QInit0, and returns the corresponding Quipper gates, together with
-- the new allocation map (the wire is initialized).
translateQInit0 :: WireAllocMap -> Operand -> TranslRes
translateQInit0 wmap op = (wmap', gates)
    where gates = translateAncilla wmap op $ QInitGate False
          wmap' = updateWireMap wmap op (initScalar, initCell)

-- | Takes as input a wire allocation map and the lhs of an assignment
-- (represented by a String for the declaration name, and a Maybe Int for the
-- optional array cell index). Assumes that the rhs of the assignment is a void
-- call to QInit1, and returns the corresponding Quipper gates, together with
-- the new allocation map (the wire is initialized).
translateQInit1 :: WireAllocMap -> Operand -> TranslRes
translateQInit1 wmap op = (wmap', gates)
    where gates = translateAncilla wmap op $ QInitGate True
          wmap' = updateWireMap wmap op (initScalar, initCell)

-- | Takes as input a wire allocation map and the lhs of an assignment
-- (represented by a String for the declaration name, and a Maybe Int for the
-- optional array cell index). Assumes that the rhs of the assignment is a void
-- call to QTerm0, and returns the corresponding Quipper gates, together with
-- the new allocation map (the wire is terminated).
translateQTerm0 :: WireAllocMap -> Operand -> TranslRes
translateQTerm0 wmap op = (wmap', gates)
    where gates = translateAncilla wmap op $ QTermGate False
          wmap' = updateWireMap wmap op (termScalar, termCell)

-- | Takes as input a wire allocation map and the lhs of an assignment
-- (represented by a String for the declaration name, and a Maybe Int for the
-- optional array cell index). Assumes that the rhs of the assignment is a void
-- call to QTerm1, and returns the corresponding Quipper gates, together with
-- the new allocation map (the wire is terminated).
translateQTerm1 :: WireAllocMap -> Operand -> TranslRes
translateQTerm1 wmap op = (wmap', gates)
    where gates = translateAncilla wmap op $ QTermGate True
          wmap' = updateWireMap wmap op (termScalar, termCell)
