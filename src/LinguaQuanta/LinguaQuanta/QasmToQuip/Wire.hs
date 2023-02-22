-- | Functions and data types to track the allocation states of OpenQASM
-- registers, and to map said registers to Quipper wires.

module LinguaQuanta.QasmToQuip.Wire
  ( CellUpdate
  , DeclType(..)
  , Eq(..)
  , ScalarUpdate
  , Show(..)
  , WireState
  , WireError(..)
  , WireAllocMap
  , allocate
  , allocateWire
  , getCellIndex
  , getDeclType
  , getScalarIndex
  , hasLoans
  , isInput
  , initialAllocations
  , initCell
  , initScalar
  , initWire
  , isOutput
  , loanWire
  , mvCellToCell
  , mvCellToScalar
  , mvScalarToCell
  , mvScalarToScalar
  , returnWire
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

import qualified Data.Set as Set
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

-- | Helper type to track loaned ancillas.
type LoanRegistry = Set.Set Int

-- | Helper type to count QWire/CWire pairings.
type TypePairRegistry = Set.Set Int

-- | Maintains a mapping from declarartions to WireStates.
data WireAllocMap = WireAllocMap Int
                                 (MaxTracker Int)
                                 LoanRegistry
                                 TypePairRegistry
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
initialAllocations = WireAllocMap 0 (initTracker 0) loans pairs map
    where loans = Set.empty
          pairs = Set.empty
          map   = Map.empty

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
allocate ty name size (WireAllocMap tot cur loans pairs map) =
    if Map.member name map
    then Nothing
    else case size of
        Nothing -> add 1 (Left $ allocateWire tot)
        Just n  -> add n (Right $ populateWires tot n)
    where add c st = Just $ WireAllocMap (tot + c)
                                         (updateTracker cur $ \x -> x + c)
                                         loans
                                         pairs
                                         (Map.insert name (ty, st) map)

-- | Takes as input a declaration name and a wire allocation map. Returns the
-- type of the declaration, according to the declaration map.
getDeclType :: String -> WireAllocMap -> DeclType
getDeclType name (WireAllocMap _ _ _ _ map) =
    case Map.lookup name map of
        Nothing                -> Undeclared
        Just (ty, Left _)      -> Scalar ty
        Just (ty, Right cells) -> Array ty (IntMap.size cells)

-- | Takes as input a declaration name and a wire allocation map. If there is a
-- scalar declaration of the same name, then returns the corresponding wire
-- index. Otherwise, returns nothing.
getScalarIndex :: String -> WireAllocMap -> Maybe Int
getScalarIndex name (WireAllocMap _ _ _ _ map) =
    case Map.lookup name map of
        Nothing           -> Nothing
        Just (_, Left st) -> Just $ wireIndex st
        Just (_, Right _) -> Nothing

-- | Takes as input a declaration identifier (name), an array index (idx), and
-- a wire allocation map. If there is an array declaration identified by name,
-- with a cell at index idx, then returns the corresponding wire index.
-- Oterwise, returns nothing.
getCellIndex :: String -> Int -> WireAllocMap -> Maybe Int
getCellIndex name idx (WireAllocMap _ _ _ _ map) =
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
type StatefulUpdate = DeclWireStateMap -> Maybe (Bool, DeclWireStateMap)

-- | Extends TryMaybeUpdate to scalars.
type ScalarUpdate = String -> TryMapUpdate

-- | Extends TryMaybeUpdate to cells.
type CellUpdate = String -> Int -> TryMapUpdate

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
applyToScalar :: WirePred -> WireUpdate -> String -> StatefulUpdate
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
initScalar :: ScalarUpdate
initScalar name (WireAllocMap tot cur loans pairs map) =
    branchJust (applyToScalar initPred initWire name map) $ \(b, map') ->
        let cur' = updateTracker cur $ \x -> x + fromEnum b
        in Just $ WireAllocMap tot cur' loans pairs map'

-- | Takes as input a declaration name (name) and a wire allocation map (map).
-- If map contains a declaration (ty, w) of type Scalar, then replaces the
-- entry (name, (ty, w)) with (name, (ty, termWire w)) to obtain map'. The size
-- of map' is decremented when w is an output. Otherwise, returns nothing.
--
-- Note: This method assumes that update is applicable to w. If the declaration
-- exists and the update is not applicable, then an error is reported.
termScalar :: ScalarUpdate
termScalar name (WireAllocMap tot cur loans pairs map) =
    branchJust (applyToScalar isOutput termWire name map) $ \(b, map') ->
        let cur' = updateTracker cur $ \x -> x - fromEnum b
        in Just $ WireAllocMap tot cur' loans pairs map'

-- | Takes as input a declaration name (name) and a wire allocation map (map).
-- If map contains a declaration (ty, w) of type Scalar, then replaces the
-- entry (name, (ty, w)) with (name, (ty, useWire w)) to obtain map'.
--
-- Note: This method assumes that update is applicable to w. If the declaration
-- exists and the update is not applicable, then an error is reported.
useScalar :: ScalarUpdate
useScalar name (WireAllocMap tot cur loans pairs map) =
    branchJust (applyToScalar isWire useWire name map) $ \(_, map') ->
        Just $ WireAllocMap tot cur loans pairs map'

-- | Helper method to interact with array declarations. Takes as input a wire
-- predicate (pred), a wire updater (update), a declaration name (name), an
-- array index (idx), and a DeclWireStateMap (map). If map contains a
-- declaration (ty, w) name of type Array, then replaces (name[idx], (ty, w))
-- with (name[idx], (ty, update w)) to obtain map', and returns (pred w, map').
-- Otherwise, returns nothing.
--
-- Note: This method assumes that update is applicable to w. If the declaration
-- exists and the update is not applicable, then an error is reported.
applyToCell :: WirePred -> WireUpdate -> String -> Int -> StatefulUpdate 
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
initCell :: CellUpdate
initCell name idx (WireAllocMap tot cur loans pairs map) =
    branchJust (applyToCell initPred initWire name idx map) $ \(b, map') ->
        let cur' = updateTracker cur $ \x -> x + fromEnum b
        in Just $ WireAllocMap tot cur' loans pairs map'

-- | Takes as input a declaration name (name), an array index (idx), and a wire
-- allocation map (map). If map contains a declaration (ty, w) of type Array of
-- size at least idx + 1, then replaces the entry (name[idx], (ty, w)) with
-- (name[idx], (ty, termWire w)) to obtain map'. The size of map' is
-- decremented when w is an output. Otherwise, returns nothing.
--
-- Note: This method assumes that update is applicable to w. If the declaration
-- exists and the update is not applicable, then an error is reported.
termCell :: CellUpdate
termCell name idx (WireAllocMap tot cur loans pairs map) =
    branchJust (applyToCell isOutput termWire name idx map) $ \(b, map') ->
        let cur' = updateTracker cur $ \x -> x - fromEnum b
        in Just $ WireAllocMap tot cur' loans pairs map'

-- | Takes as input a declaration name (name), an array index (idx) and a wire
-- allocation map (map). If map contains a declaration (ty, w) of type Array of
-- size at least idx + 1, then replaces the entry (name[idx], (ty, w)) with
-- (name[idx], (ty, useWire w)) to obtain map'.
--
-- Note: This method assumes that update is applicable to w. If the declaration
-- exists and the update is not applicable, then an error is reported.
useCell :: CellUpdate
useCell name idx (WireAllocMap tot cur loans pairs map) =
    branchJust (applyToCell isWire useWire name idx map) $ \(_, map') ->
        Just $ WireAllocMap tot cur loans pairs map'

-------------------------------------------------------------------------------
-- * Ancilla-like Interface.

-- | Takes as input a wire allocation map (wmap). Determines an unused wire w
-- in wmap. Returns wmap updated to loan w, together with w.
loanWire :: WireAllocMap -> (WireAllocMap, Int)
loanWire (WireAllocMap tot cur loans pairs map) = (wmap, tot)
    where tot'   = tot + 1
          cur'   = updateTracker cur $ \x -> x + 1
          loans' = Set.insert tot loans
          wmap   = WireAllocMap tot' cur' loans' pairs map

-- | Takes as input a wire index (w) and a wire allocation map (wmap). If w is
-- on loan from wmap, then the loan is terminated, and the updated wire
-- allocation map is returned. Otherwise, nothing is returned.
returnWire :: Int -> WireAllocMap -> Maybe WireAllocMap
returnWire w (WireAllocMap tot cur loans pairs map)
    | Set.member w loans = Just $ WireAllocMap tot cur' loans' pairs map
    | otherwise          = Nothing
    where cur'   = updateTracker cur $ \x -> x - 1
          loans' = Set.delete w loans

-- | Returns true if a wire allocation map has outstanding loans.
hasLoans :: WireAllocMap -> Bool
hasLoans (WireAllocMap _ _ loans _ _) = not $ Set.null loans

-------------------------------------------------------------------------------
-- * Measurement-based State Collapse Interface.

-- | Function to correct the wire state for the destination.
mvUpdateDecl :: WireState -> WireState
mvUpdateDecl st = WireState { wireIndex = wireIndex st
                            , initFirst = Just True
                            , termAtEnd = termAtEnd st
                            }

-- | Implementation details for mv{Cell,Scalar}ToScalar.
setScalarDecl :: Maybe WireState -> ScalarUpdate
setScalarDecl Nothing   _  _                        = Nothing
setScalarDecl (Just st) id (WireAllocMap x y z pairs map) =
    case Map.lookup id map of
        Nothing           -> Nothing
        Just (ty, Left _) -> let map'   = Map.insert id (ty, Left st) map
                                 pairs' = Set.insert (wireIndex st) pairs
                             in Just $ WireAllocMap x y z pairs' map'
        Just (_, Right _) -> Nothing

-- | Implementation details for mv{Cell,Scalar}ToCell.
setCellDecl :: Maybe WireState -> CellUpdate
setCellDecl Nothing   _  _   _                        = Nothing
setCellDecl (Just st) id idx (WireAllocMap x y z pairs map) =
    case Map.lookup id map of
        Nothing            -> Nothing
        Just (_, Left _)   -> Nothing
        Just (ty, Right c) -> if IntMap.member idx c
                              then let c'     = IntMap.insert idx st c
                                       map'   = Map.insert id (ty, Right c') map
                                       pairs' = Set.insert (wireIndex st) pairs
                                   in Just $ WireAllocMap x y z pairs' map'
                              else Nothing

-- | Implementation details for mvScalarTo{Cell,Scalar}.
getScalarDecl :: WireAllocMap -> String -> Maybe WireState
getScalarDecl (WireAllocMap _ _ _ _ map) id =
    case Map.lookup id map of
        Nothing               -> Nothing
        Just (_, Left scalar) -> Just $ mvUpdateDecl scalar
        Just (_, Right _)     -> Nothing

-- | Implementation details for mvCellTo{Cell,Scalar}.
getCellDecl :: WireAllocMap -> String -> Int -> Maybe WireState
getCellDecl (WireAllocMap _ _ _ _ map) id idx =
    case Map.lookup id map of
        Nothing           -> Nothing
        Just (_, Left _)  -> Nothing
        Just (_, Right c) -> case IntMap.lookup idx c of
            Nothing -> Nothing
            Just st -> Just $ mvUpdateDecl st

-- | Takes as input a scalar entry (String), a cell entry (String, Int), and a
-- wire allocation map (wmap). Returns an updated version of wmap where the
-- entry at (String, Int) is replaced by the entry at (String). If either entry
-- is invalid, then nothing is returned.
mvScalarToScalar :: String -> ScalarUpdate
mvScalarToScalar srcId dstId wmap =
    case setScalarDecl srcDecl dstId wmap of
        Just wmap' -> termScalar srcId wmap'
        Nothing    -> Nothing
    where srcDecl = getScalarDecl wmap srcId

-- | See mvScalarToScalar, except scalar to cell.
mvScalarToCell :: String -> CellUpdate
mvScalarToCell srcId dstId dstIdx wmap =
    case setCellDecl srcDecl dstId dstIdx wmap of
        Just wmap' -> termScalar srcId wmap'
        Nothing    -> Nothing
    where srcDecl = getScalarDecl wmap srcId

-- | See mvScalarToScalar, except cell to scalar.
mvCellToScalar :: String -> Int -> ScalarUpdate
mvCellToScalar srcId srcIdx dstId wmap =
    case setScalarDecl srcDecl dstId wmap of
        Just wmap' -> termCell srcId srcIdx wmap'
        Nothing    -> Nothing
    where srcDecl = getCellDecl wmap srcId srcIdx

-- | See mvScalarToScalar, except cell to cell.
mvCellToCell :: String -> Int -> CellUpdate
mvCellToCell srcId srcIdx dstId dstIdx wmap =
    case setCellDecl srcDecl dstId dstIdx wmap of
        Just wmap' -> termCell srcId srcIdx wmap'
        Nothing    -> Nothing
    where srcDecl = getCellDecl wmap srcId srcIdx

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
toWireSubset pred (WireAllocMap _ _ _ _ map) = Map.foldr f IntMap.empty map
    where f = wireFold pred

-- | Takes as input a wire allocation map. Returns a mapping from wire indices
-- to wire types, where each wire corresponds to a Quipper input.
toQuipperInputs :: WireAllocMap -> WireSubset
toQuipperInputs = toWireSubset isInput

-- | Takes as input a wire allocation map. Returns a mapping from wire indices
-- to wire types, where each wire corresponds to a Quipper output.
toQuipperOutputs :: WireAllocMap -> WireSubset
toQuipperOutputs = toWireSubset isOutput

-- | Returns the size of a Quipper circuit, as indicated by a WireAllocMap.
toSize :: WireAllocMap -> Int
toSize (WireAllocMap _ cur _ pairs _) = maxval cur - Set.size pairs
