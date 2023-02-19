-- | Functions to translate assignment statements from OpenQASM to Quipper.

module LinguaQuanta.QasmToQuip.Operand
  ( UpdatePair
  , getUpdateFn
  , getWire
  , toOperand
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.Operand (Operand(..))
import LinguaQuanta.QasmToQuip.Wire
  ( CellUpdate
  , ScalarUpdate
  , WireAllocMap
  , getCellIndex
  , getScalarIndex
  )

-------------------------------------------------------------------------------
-- * Converssion.

-- | Takes as input a declaration identifier (id) and optionally an index into
-- the declaration (idx). If idx is set, then (Cell id idx) is returned.
-- Otherwise, (QRef id) is returned.
toOperand :: String -> Maybe Int -> Operand
toOperand id Nothing    = QRef id
toOperand id (Just idx) = Cell id idx

-------------------------------------------------------------------------------
-- * WireAllocMap Queries.

-- | Takes as input an operand and a wire allocation map. Determines the type
-- of operand, and retrieves the corresponding wire from index from the wire
-- map. If wire lookup fails, then nothing is returned.
getWire :: Operand -> (WireAllocMap -> Maybe Int)
getWire (QRef id)     = getScalarIndex id
getWire (Cell id idx) = getCellIndex id idx

-------------------------------------------------------------------------------
-- * WireAllocMap Updates.

-- | Encodes update functions for both QRef and Cell.
type UpdatePair = (ScalarUpdate, CellUpdate)

-- | Takes as input a pair of update functions and an operand. Determines the
-- type of operand, and applies the correct update function.
getUpdateFn :: UpdatePair -> Operand -> (WireAllocMap -> Maybe WireAllocMap)
getUpdateFn (f, _) (QRef id)     = f id
getUpdateFn (_, g) (Cell id idx) = g id idx
