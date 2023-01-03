-- | Functions and data types to track the allocation states of OpenQASM
-- registers, and to map said registers to Quipper wires.

module QasmToQuip.Wire
  ( WireState
  , WireError(..)
  , WireAllocMap
  , DeclType(..)
  , Show(..)
  , Eq(..)
  , allocate
  , allocateWire
  , getDeclType
  , isInput
  , initialAllocations
  , initWire
  , isOutput
  , termWire
  , useWire
  , wireIndex
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( isJust
  , maybe
  )
import Quip.Wire (WireType)
import Utils (setMaybe)

-------------------------------------------------------------------------------
-- * Wire Allocation Data.

-- | Each OpenQASM register is mapped to a Quipper wire. The state of this wire
-- is represented by a WireState.
data WireState = WireState { wireIndex :: Int
                           , initFirst ::(Maybe Bool)
                           , termAtEnd :: Bool
                           } deriving (Show, Eq)

-- | Consumes an integer n. Initializes a new wire, with label n, that is not
-- yet initialized nor terminated. Initializing a wire from this state will
-- create an ancilla. Otherwise, the wire is taken to be an input to the
-- circuit. Likewise, the wire is taken to be an output unless the wire is
-- eventually terminated.
allocateWire :: Int -> WireState
allocateWire n = WireState { wireIndex = n
                           , initFirst = Nothing
                           , termAtEnd = False
                           }

-- | Returns true if the wire is taken as an input.
isInput :: WireState -> Bool
isInput = maybe True not . initFirst

-- | Returns true if the wire is returned as an output.
isOutput :: WireState -> Bool
isOutput = not . termAtEnd

-------------------------------------------------------------------------------
-- * Abstract Wire Operations (WireState API).

-- | Errors representing improper usage of the WireState API.
data WireError = DoubleInit
               | DoubleTerm
               | UseBeforeInit
               deriving (Show, Eq)

-- | A function that takes as input the state of a wire. If the wire update is
-- valid from the given state, then a new wire state is returned. Otherwise, a
-- wire error is returned which explains the failure.
type WireUpdate = WireState -> Either WireError WireState

-- | Internal implementation of useWire, initWire, and termWire. Consumes two
--Bbooleans, indicating initialization and termination, respectively. Returns a
-- WireUpdate function defined by the two Booleans.
updateWire :: Bool -> Bool -> WireUpdate
updateWire init term state
    -- These states should be unreachable and indicate API usage errors.
    | init && active && used = Left DoubleInit
    | term && not active     = Left DoubleTerm
    | not init && not active = Left UseBeforeInit
    -- Otherwise, update the state.
    | otherwise = Right WireState { wireIndex = wireIndex state
                                  , initFirst = setMaybe init $ initFirst state
                                  , termAtEnd = term
                                  }
    where active = isOutput state
          used   = isJust $ initFirst state

-- | Computes the state of a wire, after applying either a measurement or a
-- unitary gate. If the wire is not active, then an error is returned.
useWire :: WireUpdate
useWire = updateWire False False

-- | Computes the state of a wire, after (re)initializing it as an ancilla. If
-- the wire is already in use, then an error is returned.
initWire :: WireUpdate
initWire = updateWire True False

-- | Computes the state of a wire, after terminating it. If the wire is not in
-- use, then an error is returned.
termWire :: WireUpdate
termWire = updateWire False True

-------------------------------------------------------------------------------
-- * Declaration to Wire Mappings.

-- | Helper type to represent the WireState of a declaration (in particular,
-- either a WireState or a mapping from array indices to WireStates).
type DeclWireState = Either WireState (IntMap.IntMap WireState)

-- | Helper type to map declaration names to DeclWireStates.
type DeclWireStateMap = Map.Map String (WireType, DeclWireState)

-- | Maintains a mapping from declarartions to WireStates.
data WireAllocMap = WireAllocMap Int DeclWireStateMap deriving (Eq, Show)

-- | Helper type for fuctions taht try to update WireAllocMaps, with the
-- possibility of failure.
type TryMapUpdate = WireAllocMap -> Maybe WireAllocMap

-- | A enum to indicate whether a declaration is undeclared, or a scalar-type,
-- or of an array-type.
data DeclType = Undeclared
              | Scalar WireType
              | Array WireType Int
              deriving (Eq, Show)

-- | Returns an empty wire allocation map.
initialAllocations :: WireAllocMap
initialAllocations = WireAllocMap 0 Map.empty

-- | Helper function to allocate wires of an array.
populateWires :: Int -> Int -> IntMap.IntMap WireState
populateWires wct size = foldr f IntMap.empty [0..(size - 1)]
    where f idx = IntMap.insert idx (allocateWire $ size + idx)

-- | Takes as input a declaration identifier (name), the declaration size when
-- applicable (size), and a wire allocation map. If (name, size) describes a
-- scalar declaration (i.e., size is Nothing) not found in the map, then adds a
-- new wire to the map associated to name. If (name, size) describes an array
-- declaration (i.e., size is Just n) not found in the map, then adds new wires
-- to the map associated to each of name[0] through to name[n-1]. Otherwise,
-- nothing is returned.
allocate :: WireType -> String -> (Maybe Int) -> TryMapUpdate
allocate ty name size (WireAllocMap n map) =
    if Map.member name map
    then Nothing
    else case size of
        Nothing -> add 1 (Left $ allocateWire n)
        Just m  -> add m (Right $ populateWires n m)
    where add c st = Just $ WireAllocMap (n + c) (Map.insert name (ty, st) map)

-- | Takes as input a declaration name and a wire allocation map. Returns the
-- type of the declaration, according to the declaration map.
getDeclType :: String -> WireAllocMap -> DeclType
getDeclType name (WireAllocMap _ map) =
    case Map.lookup name map of
        Nothing                -> Undeclared
        Just (ty, Left _)      -> Scalar ty
        Just (ty, Right cells) -> Array ty (IntMap.size cells)
