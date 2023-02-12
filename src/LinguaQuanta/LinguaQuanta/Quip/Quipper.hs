-- | Converts a Quipper program into our internal representation. Also provides
-- encapsulation around the Quipper circuit interface (which is subject to
-- refactoring). This design improves maintainability, since changes to the
-- Quipper internal library will only impact this single module.

module LinguaQuanta.Quip.Quipper
  ( GateCirc(..)
  , QuipCirc(..)
  , WireType(..)
  , gatesToAscii
  , parseQuip
  , quipToGates
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Lazy as Map
import Data.Maybe (catMaybes)
import LinguaQuanta.Quip.Gate
  ( Control(..)
  , Gate(..)
  , Wire
  )
import LinguaQuanta.Quip.GateName 
  ( NamedOp(..)
  , toGateName
  , toRotName
  )
import LinguaQuanta.Quip.Wire (WireType(..))
import Quipper
  ( Circ
  , Endpoint
  , Signed(..)
  )
import Quipper.Internal.Circuit
  ( BoxId
  , Gate(..)
  , TypedSubroutine
  , Wiretype(..)
  )
import Quipper.Internal.Generic (encapsulate_generic)
import Quipper.Internal.Printing (ascii_of_bcircuit)
import Quipper.Libraries.QuipperASCIIParser (parse_circuit)

-------------------------------------------------------------------------------
-- * Circuit Representations.

-- | A gate-representation of a Quipper circuit. This is an abstract Quipper
-- circuit.
data GateCirc = GateCirc { inputs  :: IntMap.IntMap WireType
                         , gates   :: [LinguaQuanta.Quip.Gate.Gate]
                         , outputs :: IntMap.IntMap WireType
                         , size    :: Int
                         } deriving (Show, Eq)

-- | Wraps a Quipper circuit function, along with its shape. This is a concrete
-- Quipper circuit.
data QuipCirc = QuipCirc ([Endpoint] -> Circ [Endpoint]) [Endpoint]

-------------------------------------------------------------------------------
-- * Circuit Abstraction.

-- | Helper function to convert Quipper internal wire types to the wire types
-- used within the translator.
arityToWire :: Wiretype -> WireType
arityToWire Qbit = QWire
arityToWire Cbit = CWire

-- | Helper function to convert Quipper's internal signed wires to the controls
-- used within the translator.
encapsulateCtrl :: Signed Wire -> Control
encapsulateCtrl (Signed w False) = Neg w
encapsulateCtrl (Signed w True)  = Pos w

-- | Helper function to encapsulats a list of signed wires via encapsulateCtrl.
encapsulateCtrls :: [Signed Wire] -> [Control]
encapsulateCtrls = map encapsulateCtrl

-- | Consumes a list of inputs (ins), a list of general controls (gctrls), and
-- the name of an operator. If the operator is user-defined, then ctrls is
-- prepended to ins to eliminate generalized controls (this loses information
-- about the program, but is a valid transformation). Otherwise, if the
-- operator is primitive, then ctrls must be [] and ins is returned. Otherwise,
-- an error is raised.
mergeInputs :: NamedOp a => a -> [Wire] -> [Wire] -> [Wire]
mergeInputs name ins gctrls
    | isUserDefined name = gctrls ++ ins
    | gctrls == []       = ins
    | otherwise          = error errmsg
    where errmsg = "quipToGates: mergeInputs: Primitive must not have gctrls."

-- | Helper function to convert Quipper internal gates to the gates used within
-- the translator. If conversion fails, then an error is raised.
abstractGate :: Quipper.Internal.Circuit.Gate -> Maybe LinguaQuanta.Quip.Gate.Gate
abstractGate (QGate name inv ins gctrls ctrls _) = Just gate
    where tname  = toGateName name
          allins = mergeInputs tname ins gctrls
          qctrls = encapsulateCtrls ctrls
          gate   = NamedGate tname inv allins qctrls
abstractGate (QRot name inv angle ins gctrls ctrls _) = Just gate
    where tname  = toRotName name
          allins = mergeInputs tname ins gctrls
          qctrls = encapsulateCtrls ctrls
          gate   = RotGate tname inv angle allins qctrls
abstractGate (GPhase angle _ ctrls _) = Just gate
    where qctrls = encapsulateCtrls ctrls
          gate   = PhaseGate angle qctrls
abstractGate (QInit on wire _) = Just $ QInitGate on wire
abstractGate (QTerm on wire _) = Just $ QTermGate on wire
abstractGate (QDiscard wire)   = Just $ QDiscardGate wire
abstractGate (QMeas wire)      = Just $ QMeasGate wire
abstractGate (Comment _ _ _)   = Nothing

-- | Consumes the functional representation of a Quipper circuit (with optional
-- post-processing). Returns a gate-based representation of the circuit. If the
-- conversion fails, then an error is raised.
quipToGates :: QuipCirc -> GateCirc
quipToGates (QuipCirc fn sp)
    | null ns = GateCirc { inputs  = IntMap.map arityToWire ins
                         , gates   = catMaybes $ map abstractGate gates
                         , outputs = IntMap.map arityToWire outs
                         , size    = sz
                         }
    | otherwise = error "Subroutines are not supported."
    where efn msg                = "quipToGates: encapsulate_generic: " ++ msg
          (_, bcirc, _)          = encapsulate_generic efn fn sp
          (circ, ns)             = bcirc
          (ins, gates, outs, sz) = circ

-------------------------------------------------------------------------------
-- * Circuit Concretization.

-- | Helper function to convert the wire types used within translation to the
-- Quipper internal wire types.
wireToArity :: WireType -> Wiretype
wireToArity QWire = Qbit
wireToArity CWire = Cbit

-- | Helper function to convert the controls used within the translator to
-- Quipper's internal signed wires.
exposeCtrl :: Control -> Signed Wire
exposeCtrl (Neg w) = Signed w False
exposeCtrl (Pos w) = Signed w True

-- | Helper function to expose a list of controls via exposeCtrl.
exposeCtrls :: [Control] -> [Signed Wire]
exposeCtrls = map exposeCtrl

-- | Helper function to convert an abstract gate used within translation to the
-- best approximated Quipper gate.
concretizeGate :: LinguaQuanta.Quip.Gate.Gate -> Quipper.Internal.Circuit.Gate
concretizeGate (NamedGate name inv ins ctrls) = gate
    where sname  = printGate name
          qctrls = exposeCtrls ctrls
          gate   = QGate sname inv ins [] qctrls False
concretizeGate (RotGate name inv angle ins ctrls) = gate
    where sname  = printGate name
          qctrls = exposeCtrls ctrls
          gate   = QRot sname inv angle ins [] qctrls False
concretizeGate (PhaseGate angle ctrls) = gate
    where qctrls = exposeCtrls ctrls
          gate   = GPhase angle [] qctrls False
concretizeGate (QInitGate on wire) = QInit on wire False
concretizeGate (QTermGate on wire) = QTerm on wire False
concretizeGate (QDiscardGate wire) = QDiscard wire
concretizeGate (QMeasGate wire)    = QMeas wire

-- | Consumes a gate-based representation of a Quipper circuit. Returns the
-- ASCII representation of the circuit. If the conversion fails, then an error
-- is raised.
gatesToAscii :: GateCirc -> String
gatesToAscii circ = ascii_of_bcircuit bcirc
    where ins    = IntMap.map wireToArity $ inputs circ
          qgates = map concretizeGate $ gates circ
          outs   = IntMap.map wireToArity $ outputs circ
          qcirc  = (ins, qgates, outs, size circ)
          ns     = Map.empty :: Map.Map BoxId TypedSubroutine
          bcirc  = (qcirc, ns)

-------------------------------------------------------------------------------
-- * Circuit Conversion.

-- | Consumes the name of an input stream (fp) and the contents of the input
-- stream (input). If input contaisn a valid Quipper circuit in ASCII format,
-- then the Quipper circuit is returned. Otherwise, an error is raised.
parseQuip :: FilePath -> String -> QuipCirc
parseQuip fp input = QuipCirc fn sp
    where (sp, fn) = parse_circuit input
