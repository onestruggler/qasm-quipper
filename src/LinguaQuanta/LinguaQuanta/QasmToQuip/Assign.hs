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

import LinguaQuanta.Qasm.Operand (Operand)
import LinguaQuanta.QasmToQuip.Operand
  ( UpdatePair
  , getUpdateFn
  , getWire
  , toOperand
   )
import LinguaQuanta.QasmToQuip.Wire
  ( WireAllocMap
  , initCell
  , initScalar
  , termCell
  , termScalar
  )
import LinguaQuanta.Quip.Gate
  ( Gate(..)
  , Wire
  )

-------------------------------------------------------------------------------
-- * Ancilla Translation Utilities.

-- | Helper function to compute the gate for a classical ancilla statement.
-- Takes as input the current wire allocations, an operand representation of
-- the lhs, and a constructor for the ancilla (with all arguments bound except
-- for the wire).
translateAncilla :: WireAllocMap -> Operand -> (Wire -> Gate) -> [Gate]
translateAncilla wmap op ancilla =
    case getWire op wmap of
        Just w  -> [ancilla w]
        Nothing -> error $ "Failed to handle ancilla at: " ++ show op

-- | Helper function to update the wire allocation map after applying an
-- ancilla gate. Takes as input the current wire allocations, an operand
-- representation of the lhs, and a pair of update functions (one for a scalar
-- operand and one for a cell operand). 
updateWireMap :: WireAllocMap -> Operand -> UpdatePair -> WireAllocMap
updateWireMap wmap op fns =
    case getUpdateFn fns op wmap of
        Just wmap' -> wmap'
        Nothing    -> error $ "Failed to update ancilla state at: " ++ show op

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
