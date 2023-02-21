-- | Functions to simplify ancilla translations.

module LinguaQuanta.QasmToQuip.Ancilla
  ( translateAncilla
  , updateWireMap
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.Operand (Operand)
import LinguaQuanta.QasmToQuip.Operand
  ( UpdatePair
  , getUpdateFn
  , getWire
  )
import LinguaQuanta.QasmToQuip.Wire (WireAllocMap)
import LinguaQuanta.Quip.Gate
  ( Gate(..)
  , Wire
  )

-------------------------------------------------------------------------------
-- * Utilities.

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
