-- | Functions and data structures to map Quipper wires to OpenQASM 3
-- declarations.

module LinguaQuanta.QuipToQasm.Wire
  ( StateChangeErr(..)
  , WireLookup
  , allocateInputWires
  , collapseState
  , getAllocation
  , getState
  , translateQWireInputs
  , translateCWireInputs
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (isNothing)
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

-- | The prefix for bits allocation mid-translation.
_DYN_CWIRE_REG :: String
_DYN_CWIRE_REG = "shadow_cwire_"

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
                                     , state  :: WireType
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
                                 , state  = QWire
                                 }
          map' = IntMap.insert id alloc map
allocHelper ((id, CWire):pairs) qs cs map = allocHelper pairs qs (cs + 1) map'
    where alloc = DeclAllocation { qalloc = Nothing
                                 , calloc = Just $ Cell _IN_CWIRE_REG cs
                                 , state  = CWire
                                 }
          map' = IntMap.insert id alloc map

-- | Initializes a wire assignment map from the input wires of a circuit.
allocateInputWires :: IntMap.IntMap WireType -> WireLookup
allocateInputWires map = WireLookup allocs 0
    where allocs = allocHelper (IntMap.assocs map) 0 0 IntMap.empty

-- | Returns the declaration associated to a wire.
getAllocation :: WireType -> Wire -> WireLookup -> Maybe Operand
getAllocation ty id (WireLookup map _) =
    case IntMap.lookup id map of
        Nothing    -> Nothing
        Just entry -> case ty of
            QWire -> qalloc entry
            CWire -> calloc entry

-- | Returns the current state of the wire.
getState :: Wire -> WireLookup -> Maybe WireType
getState id (WireLookup map _) = branchJust (IntMap.lookup id map) f
    where f = Just . state

-------------------------------------------------------------------------------
-- * Update Wire States.

-- | Describes failures that can occur during type conversion and state change.
data StateChangeErr = UnallocatedWire Wire
                    | BadWireState WireType
                    deriving (Show, Eq)

-- | The return-value of a function that changes state.
type StateChangeRes a = Either a StateChangeErr

-- | The underlying type of a function that changes state.
type StateChangeFn a = WireLookup -> StateChangeRes a

-- | Takes as input the number of dynamically allocated bits (n) and a
-- declaration allocation record (alloc). This function determines the required
-- declarations to convert decl from a qunatum bit to a classical bit. If decl
-- is already classical, then an error is returned. If decl already has a
-- classical allocation, then nothing is returned. Otherwise, the name of the
-- next available dynamic allocation is returned.
nameClassicalDecl :: Int -> DeclAllocation -> StateChangeRes (Maybe String)
nameClassicalDecl n alloc
    | state alloc == CWire     = Right $ BadWireState CWire
    | isNothing $ calloc alloc = Left $ Just $ _DYN_CWIRE_REG ++ show n
    | otherwise                = Left Nothing

-- | Takes as input a declaration allocation record (alloc) and (optionally)
-- the name of a dynamically allocated bit (name). If name is provided, then
-- (calloc alloc) is updated to reference name. In either case, (state alloc)
-- is updated to CWire. The new declaration allocation record is returned.
toClassical :: DeclAllocation -> Maybe String -> DeclAllocation
toClassical alloc Nothing = DeclAllocation { qalloc = qalloc alloc
                                           , calloc = calloc alloc
                                           , state  = CWire
                                           }
toClassical alloc (Just name) = DeclAllocation { qalloc = qalloc alloc
                                               , calloc = Just $ QRef name
                                               , state  = CWire
                                               }

-- | Takes as input a wire identifier (id) and a wire lookup. If id is not in
-- the wire lookup, or if id is in a classical state, then an error is returned
-- accordingly. Otherwise, id is collapsed to a classical state and the new
-- wire lookup is returned. If the wire lookup does not map id to a classical
-- bit, then the name of a new classical bit is also returned.
collapseState :: Wire -> StateChangeFn (WireLookup, Maybe String)
collapseState id (WireLookup map n) =
    case IntMap.lookup id map of
        Nothing    -> Right $ UnallocatedWire id
        Just entry -> case nameClassicalDecl n entry of
            Right err -> Right err
            Left name -> let entry' = toClassical entry name
                             map'   = IntMap.insert id entry' map
                             n'     = if isNothing name then n else n + 1
                         in Left (WireLookup map' n', name)
