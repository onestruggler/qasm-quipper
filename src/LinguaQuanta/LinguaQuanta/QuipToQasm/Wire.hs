-- | Functions and data structures to map Quipper wires to OpenQASM 3
-- declarations.

module LinguaQuanta.QuipToQasm.Wire
  ( StateChangeErr(..)
  , WireLookup
  , allocateInputWires
  , collapseState
  , getAllocation
  , getState
  , initCBit
  , initQBit
  , termCBit
  , termQBit
  , translateQWireInputs
  , translateCWireInputs
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import qualified Data.IntMap.Strict as IntMap
import Data.Maybe
  ( fromJust
  , isNothing
  )
import LinguaQuanta.Maybe (branchJust)
import LinguaQuanta.Qasm.AST (AstStmt(..))
import LinguaQuanta.Qasm.Gate (Operand(..))
import LinguaQuanta.Quip.Gate (Wire)
import LinguaQuanta.Quip.Wire (WireType(..))

-------------------------------------------------------------------------------
-- * Functions to Translate Inputs.

-- | The global register for all input qubits.
_IN_QWIRE_REG :: String
_IN_QWIRE_REG = "input_qwires"

-- | The global register for all input classical bits.
_IN_CWIRE_REG :: String
_IN_CWIRE_REG = "input_cwires"

-- | The prefix for classical bits allocation mid-translation.
_DYN_CWIRE_REG :: String
_DYN_CWIRE_REG = "shadow_cwire_"

-- | The prefix for quantum bits allocation mid-translation.
_DYN_QWIRE_REG :: String
_DYN_QWIRE_REG = "shadow_qwire_"

-- | Takes as input the number of input qubits in a Quipper circuit. Returns a
-- list of AST statements to declare the corresponding register.
translateQWireInputs :: Int -> [AstStmt]
translateQWireInputs 0 = []
translateQWireInputs n = [AstQubitDecl (Just n) _IN_QWIRE_REG]

-- | Consumes the number of input classical bits in a Quipper circuit. Returns
-- a list of AST statements to declare the corresponding register.
translateCWireInputs :: Int -> [AstStmt]
translateCWireInputs 0 = []
translateCWireInputs n = [AstBitDecl (Just n) _IN_CWIRE_REG]

-------------------------------------------------------------------------------
-- * Functions to Allocate Inputs.

-- | The OpenQASM variables corresponding to a Quipper wire. Note that a
-- Quipper wire may have both a qalloc and a calloc if type conversions occur.
data DeclAllocation = DeclAllocation { qalloc :: Maybe Operand
                                     , calloc :: Maybe Operand
                                     , state  :: Maybe WireType
                                     } deriving (Show, Eq)

-- | Maps wire identifies to their corresponding OpenQASM variables.
type AllocMap = IntMap.IntMap DeclAllocation

-- | Maps Quipper wires to OpenQASM variables.
data WireLookup = WireLookup AllocMap Int

-- | Helper function to implement allocateInputWires recursively. The function
-- converts a list of identifier/wire-type pairs into an AllocMap. The function
-- takes as input the list of remaining pairs, the number of QWires processed
-- so far (qs), the number of QWires processed so far (cs), and the allocation
-- map with qs QWire entries, and cs CWire entries (map). Returns the map
-- obtained by adding all pairs to map.
--
-- Expected Usage: allocHelper (assocs inputs) 0 0 IntMap.empty
allocHelper :: [(Wire, WireType)] -> Int -> Int -> AllocMap -> AllocMap
allocHelper []        _  _  map = map
allocHelper ((id, QWire):pairs) qs cs map = allocHelper pairs (qs + 1) cs map'
    where alloc = DeclAllocation { qalloc = Just $ Cell _IN_QWIRE_REG qs
                                 , calloc = Nothing
                                 , state  = Just QWire
                                 }
          map' = IntMap.insert id alloc map
allocHelper ((id, CWire):pairs) qs cs map = allocHelper pairs qs (cs + 1) map'
    where alloc = DeclAllocation { qalloc = Nothing
                                 , calloc = Just $ Cell _IN_CWIRE_REG cs
                                 , state  = Just CWire
                                 }
          map' = IntMap.insert id alloc map

-- | Initializes a wire assignment map from the input wires of a circuit.
allocateInputWires :: IntMap.IntMap WireType -> WireLookup
allocateInputWires map = WireLookup allocs 0
    where allocs = allocHelper (IntMap.assocs map) 0 0 IntMap.empty

-- | Returns the declaration associated to a wire.
getAllocation :: WireType -> Wire -> WireLookup -> Maybe Operand
getAllocation ty id (WireLookup map _) =
    branchJust (IntMap.lookup id map) $ \entry -> case ty of
        QWire -> qalloc entry
        CWire -> calloc entry

-- | Returns the current state of the wire.
getState :: Wire -> WireLookup -> Maybe WireType
getState id (WireLookup map _) = branchJust (IntMap.lookup id map) state

-------------------------------------------------------------------------------
-- * Update Wire States.

-- | Describes failures that can occur during type conversion and state change.
data StateChangeErr = UnallocatedWire Wire
                    | BadWireState WireType
                    | WireInactive
                    | DoubleInit Wire
                    deriving (Show, Eq)

-- | The return-value of a function that changes state.
type StateChangeRes a = Either a StateChangeErr

-- | The underlying type of a function that changes state.
type StateChangeFn a = WireLookup -> StateChangeRes a

-- | A tuple containing an updated wire lookup, and optionally, the name of a
-- new declaration that was added to the lookup.
type MaybeNDecl = (WireLookup, Maybe String)

-- | Takes as input the number of dynamically allocated bits (n), a wire type
-- (ty), and a declaration allocation record (alloc). If alloc does not have an
-- associated declaration of type ty, then a dynamic declaration is returned,
-- as determined by n. Otherwise, nothing is returned.
getDynDecl :: WireType -> Int -> DeclAllocation -> Maybe String
getDynDecl ty n alloc
    | isNothing $ declFun alloc = Just $ declStr ++ show n
    | otherwise                 = Nothing
    where isCWire = ty == CWire
          declFun = if isCWire then calloc else qalloc
          declStr = if isCWire then _DYN_CWIRE_REG else _DYN_QWIRE_REG
           

-- | Takes as input the number of dynamically allocated bits (n) and a
-- declaration allocation record (alloc). This function determines the required
-- declarations to convert decl from a qunatum bit to a classical bit. If decl
-- is already classical, then an error is returned. If decl already has a
-- classical allocation, then nothing is returned. Otherwise, the name of the
-- next available dynamic allocation is returned.
nameClassicalDecl :: Int -> DeclAllocation -> StateChangeRes (Maybe String)
nameClassicalDecl n alloc
    | state alloc == Just CWire = Right $ BadWireState CWire
    | state alloc == Nothing    = Right WireInactive
    | otherwise                 = Left $ getDynDecl CWire n alloc

-- | Takes as input a target wire type (ty), a declaration allocation record
-- (alloc), and (optionally) the name of a dynamically allocated bit (name). If
-- a name is provided, then the allocation of type ty (either calloc or qalloc)
-- is updated to reference name. In either case, (state alloc) is updated to
-- ty. The new declaration allocation record is returned.
updateAlloc :: WireType -> DeclAllocation -> Maybe String -> DeclAllocation
updateAlloc ty alloc Nothing = DeclAllocation { qalloc = qalloc alloc
                                              , calloc = calloc alloc
                                              , state  = Just ty
                                              }
updateAlloc QWire alloc (Just name) = DeclAllocation { qalloc = Just $ QRef name
                                                     , calloc = calloc alloc
                                                     , state  = Just QWire
                                                     }
updateAlloc CWire alloc (Just name) = DeclAllocation { qalloc = qalloc alloc
                                                     , calloc = Just $ QRef name
                                                     , state  = Just CWire
                                                     }

-- | Takes as input a wire type (ty), a wire identifier (id), the decalaration
-- allocation associated with id (alloc), and a partially complete MaybeNDecl.
-- By partially complete, it is assumed that the WireLookup within MaybeNDecl
-- has not yet assigned id to state ty, nor has id been updated to reference
-- the new declaration (when the optional declaration name is provided).
-- Returns a new MaybeNDecl with the name unchanged, and the WireLookup updated
-- to reflect these changes.
updateMap :: WireType -> Wire -> DeclAllocation -> MaybeNDecl -> MaybeNDecl
updateMap ty id alloc (WireLookup map n, name) = (WireLookup map' n', name)
    where alloc' = updateAlloc ty alloc name
          map'   = IntMap.insert id alloc' map
          n'     = if isNothing name then n else n + 1

-- | Takes as input a wire identifier (id) and a wire lookup. If id is not in
-- the wire lookup, or if id is in a classical state, then an error is returned
-- accordingly. Otherwise, id is collapsed to a classical state and the new
-- wire lookup is returned. If the wire lookup does not map id to a classical
-- bit, then the name of a new classical bit is also returned.
collapseState :: Wire -> StateChangeFn MaybeNDecl
collapseState id lookup@(WireLookup map n) =
    case IntMap.lookup id map of
        Nothing    -> Right $ UnallocatedWire id
        Just entry -> case nameClassicalDecl n entry of
            Right err -> Right err
            Left name -> Left $ updateMap CWire id entry (lookup, name)

-- | Takes as input a wire type (ty) and a declaration allocation (alloc). If
-- alloc is in state ty, then alloc is set to the inactive state and returned.
-- Otherwise, an error is returned describing the actual type of alloc.
toInactive :: WireType -> DeclAllocation -> StateChangeRes DeclAllocation
toInactive ty alloc
    | cur == Just ty = Left res
    | cur == Nothing = Right WireInactive
    | otherwise      = Right $ BadWireState $ fromJust cur
    where cur = state alloc
          res = DeclAllocation (qalloc alloc) (calloc alloc) Nothing

-- | Takes as input a wire type (ty), a wire identifier (id), and a wire lookup
-- (map). If id is allocated in map, and is in state ty, then a new wire lookup
-- map is returned with the state of id set to inactive, and all other entries
-- left unchanged. If the wire is undeclared, or in an inappropriate state,
-- then an appropriate error is returned.
termBitByType :: WireType -> Wire -> StateChangeFn WireLookup
termBitByType ty id (WireLookup map n) =
    case IntMap.lookup id map of
        Nothing    -> Right $ UnallocatedWire id
        Just entry -> case toInactive ty entry of
            Right err   -> Right err
            Left entry' -> let map' = IntMap.insert id entry' map
                           in Left $ WireLookup map' n

-- | Specializes termBitByType to classical wires.
termCBit :: Wire -> StateChangeFn WireLookup
termCBit = termBitByType CWire

-- | Specializes termBitByType to quantum wires.
termQBit :: Wire -> StateChangeFn WireLookup
termQBit = termBitByType QWire

-- | Takes as input a wire identifier (id) and an allocation map (map). If id
-- is recorded in map, then its allocation is returned. Otherwise, a "default"
-- allocation is returned with no declarations and an inactive state.
getAllocationOrDefault :: Wire -> AllocMap -> DeclAllocation
getAllocationOrDefault id map =
    case IntMap.lookup id map of
        Just entry -> entry
        Nothing    -> DeclAllocation Nothing Nothing Nothing

-- | Takes as input a wire type (ty), a wire identifier (id), and a wire lookup
-- (map). If id is allocated in map, and is in the inactive state, then a new
-- wire lookup map is returned with the state of id set to ty, and all other
-- entries left unchanged. If the wire is undeclared, or in an inappropriate
-- state, then an appropriate error is returned.
initBitByType :: WireType -> Wire -> StateChangeFn (WireLookup, Maybe String)
initBitByType ty id lookup@(WireLookup map n)
    | isNothing $ state entry = Left $ updateMap ty id entry (lookup, name)
    | otherwise               = Right $ DoubleInit id
    where entry = getAllocationOrDefault id map
          name  = getDynDecl ty n entry

-- | Specializes initBitByType to classical wires.
initCBit :: Wire -> StateChangeFn (WireLookup, Maybe String)
initCBit = initBitByType CWire

-- | Specializes initBitByType to quantum wires.
initQBit :: Wire -> StateChangeFn (WireLookup, Maybe String)
initQBit = initBitByType QWire
