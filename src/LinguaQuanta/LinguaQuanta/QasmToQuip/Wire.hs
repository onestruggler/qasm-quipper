-- | Functions and data types to track the allocation states of OpenQASM
-- registers, and to map said registers to Quipper wires.

module LinguaQuanta.QasmToQuip.Wire
  ( WireState
  , WireError(..)
  , WireAllocMap
  , DeclType(..)
  , Show(..)
  , Eq(..)
  , allocate
  , allocateWire
  , getCellIndex
  , getDeclType
  , getScalarIndex
  , isInput
  , initialAllocations
  , initCell
  , initScalar
  , initWire
  , isOutput
  , termCell
  , termScalar
  , termWire
  , toQuipperInputs
  , toQuipperOutputs
  , toSize
  , useCell
  , useScalar
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
import LinguaQuanta.MaxTracker
  ( MaxTracker
  , initTracker
  , maxval
  , updateTracker
  )
import LinguaQuanta.Maybe
  ( branchJust
  , setMaybe
  )
import LinguaQuanta.Quip.Wire (WireType)

-------------------------------------------------------------------------------
-- * Wire Allocation Data.

-- | Each OpenQASM register is mapped to a Quipper wire. The state of this wire
-- is represented by a WireState.
data WireState = WireState { wireIndex :: Int
                           , initFirst ::(Maybe Bool)
                           , termAtEnd :: Bool
                           } deriving (Show, Eq)

-- | A predicate over wire states.
type WirePred = WireState -> Bool

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

-- | Returns true for all wires.
isWire :: WirePred
isWire _ = True

-- | Returns true if the wire is taken as an input.
isInput :: WirePred
isInput = maybe True not . initFirst

-- | Returns true if the wire is returned as an output.
isOutput :: WirePred
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
data WireAllocMap = WireAllocMap Int
                                 (MaxTracker Int)
                                 DeclWireStateMap
                                 deriving (Eq, Show)

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
initialAllocations = WireAllocMap 0 (initTracker 0) Map.empty

-- | Helper function to allocate wires of an array.
populateWires :: Int -> Int -> IntMap.IntMap WireState
populateWires wct size = foldr f IntMap.empty [0..(size - 1)]
    where f idx = IntMap.insert idx (allocateWire $ wct + idx)

-- | Takes as input a declaration identifier (name), the declaration size when
-- applicable (size), and a wire allocation map. If (name, size) describes a
-- Scalar declaration (i.e., size is Nothing) not found in the map, then adds a
-- new wire to the map associated to name. If (name, size) describes an array
-- declaration (i.e., size is Just n) not found in the map, then adds new wires
-- to the map associated to each of name[0] through to name[n-1]. Otherwise,
-- nothing is returned.
allocate :: WireType -> String -> (Maybe Int) -> TryMapUpdate
allocate ty name size (WireAllocMap tot cur map) =
    if Map.member name map
    then Nothing
    else case size of
        Nothing -> add 1 (Left $ allocateWire tot)
        Just n  -> add n (Right $ populateWires tot n)
    where add c st = Just $ WireAllocMap (tot + c)
                                         (updateTracker cur $ \x -> x + c) 
                                         (Map.insert name (ty, st) map)

-- | Takes as input a declaration name and a wire allocation map. Returns the
-- type of the declaration, according to the declaration map.
getDeclType :: String -> WireAllocMap -> DeclType
getDeclType name (WireAllocMap _ _ map) =
    case Map.lookup name map of
        Nothing                -> Undeclared
        Just (ty, Left _)      -> Scalar ty
        Just (ty, Right cells) -> Array ty (IntMap.size cells)

-- | Takes as input a declaration name and a wire allocation map. If there is a
-- scalar declaration of the same name, then returns the corresponding wire
-- index. Otherwise, returns nothing.
getScalarIndex :: String -> WireAllocMap -> Maybe Int
getScalarIndex name (WireAllocMap _ _ map) =
    case Map.lookup name map of
        Nothing           -> Nothing
        Just (_, Left st) -> Just $ wireIndex st
        Just (_, Right _) -> Nothing

-- | Takes as input a declaration identifier (name), an array index (idx), and
-- a wire allocation map. If there is an array declaration identified by name,
-- with a cell at index idx, then returns the corresponding wire index.
-- Oterwise, returns nothing.
getCellIndex :: String -> Int -> WireAllocMap -> Maybe Int
getCellIndex name idx (WireAllocMap _ _ map) =
    case Map.lookup name map of
        Nothing               -> Nothing
        Just (_, Left _)      -> Nothing
        Just (_, Right cells) -> case IntMap.lookup idx cells of
            Nothing -> Nothing
            Just st -> Just $ wireIndex st

-------------------------------------------------------------------------------
-- * Update Declarations in WireAllocMaps.

-- | A function that updates a single cell in a DeclWireStateMap. If the update
-- fails, then nothing is returned. Otherwise, returns the new map, and a flag
-- indicating whether the update changes the current circuit size.
type ScalarUpdate = DeclWireStateMap -> Maybe (Bool, DeclWireStateMap)

-- | Helper predicate for init updates.
initPred = not. isOutput

-- | Helper method to apply a wire update to a wire state. Assumes that the
-- update is valid, and reports an error otherwise. The declaration name and
-- (when applicable) index are used in error reporting.
applyUpdate :: String -> Maybe Int -> WireUpdate -> WireState -> WireState
applyUpdate name maybeIdx update wire =
    case update wire of
        Left DoubleInit    -> error $ "Double initialization at: " ++ decl
        Left DoubleTerm    -> error $ "Double termination at: " ++ decl
        Left UseBeforeInit -> error $ "Use before initialization at: " ++ decl
        Right wire'        -> wire'
    where decl = maybe name (\idx -> name ++ "[" ++ show idx ++ "]") maybeIdx

-- | Helper method to interact with scalar declarations. Takes as input a wire
-- predicate (pred), a wire updater (update), a declaration name (name), and a
-- DeclWireStateMap (map). If map contains a declaration (ty, w) name of type
-- Scalar, then replaces (name, (ty, w)) with (name, (ty, update w)) to obtain
-- map', and returns (pred w, map'). Otherwise, returns nothing.
--
-- Note: This method assumes that update is applicable to w. If the declaration
-- exists and the update is not applicable, then an error is reported.
applyToScalar :: WirePred -> WireUpdate -> String -> ScalarUpdate
applyToScalar pred update name map =
    branchJust (Map.lookup name map) $ \(ty, st) -> case st of
        Right _   -> Nothing
        Left wire -> let wire' = applyUpdate name Nothing update wire
                         map'  = Map.insert name (ty, Left wire') map
                     in Just (pred wire, map')

-- | Takes as input a declaration name (name) and a wire allocation map (map).
-- If map contains a declaration (ty, w) of type Scalar, then replaces the
-- entry (name, (ty, w)) with (name, (ty, initWire w)) to obtain map'. The size
-- of map' is incremented when w is not an output. Otherwise, returns nothing.
--
-- Note: This method assumes that update is applicable to w. If the declaration
-- exists and the update is not applicable, then an error is reported.
initScalar :: String -> TryMapUpdate
initScalar name (WireAllocMap tot cur map) =
    branchJust (applyToScalar initPred initWire name map) $ \(b, map') ->
        let cur' = updateTracker cur $ \x -> x + fromEnum b
        in Just $ WireAllocMap tot cur' map'

-- | Takes as input a declaration name (name) and a wire allocation map (map).
-- If map contains a declaration (ty, w) of type Scalar, then replaces the
-- entry (name, (ty, w)) with (name, (ty, termWire w)) to obtain map'. The size
-- of map' is decremented when w is an output. Otherwise, returns nothing.
--
-- Note: This method assumes that update is applicable to w. If the declaration
-- exists and the update is not applicable, then an error is reported.
termScalar :: String -> TryMapUpdate
termScalar name (WireAllocMap tot cur map) =
    branchJust (applyToScalar isOutput termWire name map) $ \(b, map') ->
        let cur' = updateTracker cur $ \x -> x - fromEnum b
        in Just $ WireAllocMap tot cur' map'

-- | Takes as input a declaration name (name) and a wire allocation map (map).
-- If map contains a declaration (ty, w) of type Scalar, then replaces the
-- entry (name, (ty, w)) with (name, (ty, useWire w)) to obtain map'.
--
-- Note: This method assumes that update is applicable to w. If the declaration
-- exists and the update is not applicable, then an error is reported.
useScalar :: String -> TryMapUpdate
useScalar name (WireAllocMap tot cur map) =
    branchJust (applyToScalar isWire useWire name map) $ \(_, map') ->
        Just $ WireAllocMap tot cur map'

-- | Helper method to interact with array declarations. Takes as input a wire
-- predicate (pred), a wire updater (update), a declaration name (name), an
-- array index (idx), and a DeclWireStateMap (map). If map contains a
-- declaration (ty, w) name of type Array, then replaces (name[idx], (ty, w))
-- with (name[idx], (ty, update w)) to obtain map', and returns (pred w, map').
-- Otherwise, returns nothing.
--
-- Note: This method assumes that update is applicable to w. If the declaration
-- exists and the update is not applicable, then an error is reported.
applyToCell :: WirePred -> WireUpdate -> String -> Int -> ScalarUpdate 
applyToCell pred update name idx map =
    branchJust (Map.lookup name map) $ \(ty, st) -> case st of
        Left _     -> Nothing
        Right cell -> branchJust (IntMap.lookup idx cell) $ \wire ->
            let wire'  = applyUpdate name (Just idx) update wire
                cells' = IntMap.insert idx wire' cell
                map'   = Map.insert name (ty, Right cells') map
            in Just (pred wire, map')

-- | Takes as input a declaration name (name), an array index (idx), and a wire
-- allocation map (map). If map contains a declaration (ty, w) of type Array of
-- size at least idx + 1, then replaces the entry (name[idx], (ty, w)) with
-- (name[idx], (ty, initWire w)) to obtain map'. The size of map' is incremented
-- when w is not an output. Otherwise, returns nothing.
--
-- Note: This method assumes that update is applicable to w. If the declaration
-- exists and the update is not applicable, then an error is reported.
initCell :: String -> Int -> TryMapUpdate
initCell name idx (WireAllocMap tot cur map) =
    branchJust (applyToCell initPred initWire name idx map) $ \(b, map') ->
        let cur' = updateTracker cur $ \x -> x + fromEnum b
        in Just $ WireAllocMap tot cur' map'

-- | Takes as input a declaration name (name), an array index (idx), and a wire
-- allocation map (map). If map contains a declaration (ty, w) of type Array of
-- size at least idx + 1, then replaces the entry (name[idx], (ty, w)) with
-- (name[idx], (ty, termWire w)) to obtain map'. The size of map' is
-- decremented when w is an output. Otherwise, returns nothing.
--
-- Note: This method assumes that update is applicable to w. If the declaration
-- exists and the update is not applicable, then an error is reported.
termCell :: String -> Int -> TryMapUpdate
termCell name idx (WireAllocMap tot cur map) =
    branchJust (applyToCell isOutput termWire name idx map) $ \(b, map') ->
        let cur' = updateTracker cur $ \x -> x - fromEnum b
        in Just $ WireAllocMap tot cur' map'

-- | Takes as input a declaration name (name), an array index (idx) and a wire
-- allocation map (map). If map contains a declaration (ty, w) of type Array of
-- size at least idx + 1, then replaces the entry (name[idx], (ty, w)) with
-- (name[idx], (ty, useWire w)) to obtain map'.
--
-- Note: This method assumes that update is applicable to w. If the declaration
-- exists and the update is not applicable, then an error is reported.
useCell :: String -> Int -> TryMapUpdate
useCell name idx (WireAllocMap tot cur map) =
    branchJust (applyToCell isWire useWire name idx map) $ \(_, map') ->
        Just $ WireAllocMap tot cur map'

-------------------------------------------------------------------------------
-- * Conversions From WireAllocMaps to Quipper IO.

-- | A mapping from wire indices to wire types.
type WireSubset = IntMap.IntMap WireType

-- | Helper function to add a wire to the subset, if and only if it satisfies
-- the predicate which defines the subset.
addToSubset :: WirePred -> WireType -> WireState -> WireSubset -> WireSubset
addToSubset pred ty wire subset = if pred wire
                                  then IntMap.insert (wireIndex wire) ty subset
                                  else subset

-- | Helper function to fold over the wire state of each declaration. Each wire
-- is added to the subset, if and only if it satisfies the predicate which
-- defines the subset.
wireFold :: WirePred -> (WireType, DeclWireState) -> WireSubset -> WireSubset
wireFold pred (ty, (Left wire))   subset = addToSubset pred ty wire subset
wireFold pred (ty, (Right cells)) subset = IntMap.foldr f subset cells
    where f = addToSubset pred ty

-- | Helper function to compute the subset of a WireAllocMap defined by a
-- wire predicate. For each wire in the allocation map, if the wire satisfies
-- the predicate, then (n, ty) is added to the subset, where n is the wire
-- index and ty is the type associated with the wire.
toWireSubset :: WirePred -> WireAllocMap -> WireSubset
toWireSubset pred (WireAllocMap _ _ map) = Map.foldr f IntMap.empty map
    where f = wireFold pred

-- | Takes as input a wire allocation map. Returns a mapping from wire indices
-- to wire types, where each wire corresponds to a Quipper input.
toQuipperInputs :: WireAllocMap -> WireSubset
toQuipperInputs = toWireSubset isInput

-- | Takes as input a wire allocation map. Returns a mapping from wire indices
-- to wire types, where each wire corresponds to a Quipper output.
toQuipperOutputs :: WireAllocMap -> IntMap.IntMap WireType
toQuipperOutputs = toWireSubset isOutput

-- | Returns the size of a Quipper circuit, as indicated by a WireAllocMap.
toSize :: WireAllocMap -> Int
toSize (WireAllocMap _ cur _) = maxval cur
