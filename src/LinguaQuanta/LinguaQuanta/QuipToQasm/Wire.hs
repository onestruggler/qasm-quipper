-- | Functions and data structures to map Quipper wires to OpenQASM 3
-- declarations.

module LinguaQuanta.QuipToQasm.Wire
  ( WireLookup
  , allocateInputWires
  , getAllocation
  , getState
  , translateQWireInputs
  , translateCWireInputs
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import qualified Data.IntMap.Strict as IntMap
import LinguaQuanta.Maybe (branchJust)
import LinguaQuanta.Qasm.AST (AstStmt(..))
import LinguaQuanta.Qasm.Gate (Operand(..))
import LinguaQuanta.Quip.Wire (WireType(..))

-------------------------------------------------------------------------------
-- * Functions to Translate Inputs

-- | The global register for all input qubits.
_IN_QWIRE_REG :: String
_IN_QWIRE_REG = "input_qwires"

-- | The global register for all input classical bits.
_IN_CWIRE_REG :: String
_IN_CWIRE_REG = "input_cwires"

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
-- * Functions to Translate Inputs

-- | The OpenQASM variables corresponding to a Quipper wire. Note that a
-- Quipper wire may have both a qalloc and a calloc if type conversions occur.
data DeclAllocation = DeclAllocation { qalloc :: Maybe Operand
                                     , calloc :: Maybe Operand
                                     , state  :: WireType
                                     } deriving (Show, Eq)

-- | Maps wire identifies to their corresponding OpenQASM variables.
type AllocMap = IntMap.IntMap DeclAllocation

-- | Maps Quipper wires to OpenQASM variables.
data WireLookup = WireLookup AllocMap

-- | Helper function to implement allocateInputWires recursively. The function
-- converts a list of identifier/wire-type pairs into an AllocMap. The function
-- takes as input the list of remaining pairs, the number of QWires processed
-- so far (qs), the number of QWires processed so far (cs), and the allocation
-- map with qs QWire entries, and cs CWire entries (map). Returns the map
-- obtained by adding all pairs to map.
--
-- Expected Usage: allocHelper (assocs inputs) 0 0 IntMap.empty
allocHelper :: [(Int, WireType)] -> Int -> Int -> AllocMap -> AllocMap
allocHelper []        _  _  map = map
allocHelper ((id, QWire):pairs) qs cs map = allocHelper pairs (qs + 1) cs map'
    where alloc = DeclAllocation { qalloc = Just (Cell _IN_QWIRE_REG qs)
                                 , calloc = Nothing
                                 , state  = QWire
                                 }
          map' = IntMap.insert id alloc map
allocHelper ((id, CWire):pairs) qs cs map = allocHelper pairs qs (cs + 1) map'
    where alloc = DeclAllocation { qalloc = Nothing
                                 , calloc = Just (Cell _IN_CWIRE_REG cs)
                                 , state  = CWire
                                 }
          map' = IntMap.insert id alloc map

-- | Initializes a wire assignment map from the input wires of a circuit.
allocateInputWires :: IntMap.IntMap WireType -> WireLookup
allocateInputWires map = WireLookup allocs
    where allocs = allocHelper (IntMap.assocs map) 0 0 IntMap.empty

-- | Returns the declaration associated to a wire.
getAllocation :: WireType -> Int -> WireLookup -> Maybe Operand
getAllocation ty id (WireLookup map) =
    case IntMap.lookup id map of
        Nothing    -> Nothing
        Just entry -> case ty of
            QWire -> qalloc entry
            CWire -> calloc entry

-- | Returns the current state of the wire.
getState :: Int -> WireLookup -> Maybe WireType
getState id (WireLookup map) = branchJust (IntMap.lookup id map) (Just . state)
