-- | Provides types and functions to work with wires in Quipper.

module Quip.Wire
  ( WireType(..)
  , countCWires
  , countQWires
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import qualified Data.IntMap.Strict as IntMap

-------------------------------------------------------------------------------
-- * Wire Type.

-- | An input or output to a Quipper circuit.
data WireType = QWire | CWire deriving (Show, Eq)

-------------------------------------------------------------------------------
-- * Wire Counting Utilities.

-- | Consumes a wire type and an IntMap of WireTypes. Returns the number of
-- wires in the map of the given type.
countImpl :: WireType -> IntMap.IntMap WireType -> Int
countImpl ty = IntMap.foldr fn 0
    where fn wire n = if wire == ty then n + 1 else n

-- | Returns the number of QWires in an IntMap of WireTypes.
countQWires :: IntMap.IntMap WireType -> Int
countQWires = countImpl QWire

-- | Returns the number of QWires in an IntMap of WireTypes.
countCWires :: IntMap.IntMap WireType -> Int
countCWires = countImpl CWire
