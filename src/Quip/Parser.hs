-- | Converts a Quipper program into our internal representation. Also provides
-- encapsulation around the Quipper internal libraries. This design improves
-- maintainability, since changes to the Quipper internal library will only
-- impact this single module.

module Quip.Parser
  ( WireType(..)
  , GateCirc(..)
  , QuipCirc(..)
  , parseQuip
  , quipToGates
  ) where

import qualified Data.IntMap.Strict as IntMap
import Quip.Gate (Control(..), Gate(..), Wire)
import Quip.GateName (NamedOp(..), toGateName, toRotName)
import Quipper (Endpoint, Circ)
import Quipper.Internal.Circuit (Gate(..), Signed(..), Wiretype(..))
import Quipper.Internal.Generic (encapsulate_generic)
import Quipper.Libraries.QuipperASCIIParser (parse_circuit)

-------------------------------------------------------------------------------
-- * Gate-based Circuit Representation.

-- \ An input or output to a Quipper circuit.
data WireType = QWire | CWire deriving (Show, Eq)

-- | A gate-representation of a Quipper circuit.
data GateCirc = GateCirc { inputs  :: IntMap.IntMap Quip.Parser.WireType
                         , gates   :: [Quip.Gate.Gate]
                         , outputs :: IntMap.IntMap Quip.Parser.WireType
                         , size    :: Int
                         } deriving (Show, Eq)

-------------------------------------------------------------------------------
-- * Functional Circuit Representation.

-- | Wraps a Quipper circuit function, along with its shape.
data QuipCirc = QuipCirc ([Endpoint] -> Circ [Endpoint]) [Endpoint]

-------------------------------------------------------------------------------
-- * Gate Conversion.

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
abstractGate :: Quipper.Internal.Circuit.Gate -> Quip.Gate.Gate
abstractGate (QGate name inv ins gctrls ctrls _) = tgate
    where tname = toGateName name
          tgins = mergeInputs tname ins gctrls
          tctrl = encapsulateCtrls ctrls
          tgate = NamedGate tname inv tgins tctrl
abstractGate (QRot name inv angle ins gctrls ctrls _) = tgate
    where tname = toRotName name
          tgins = mergeInputs tname ins gctrls
          tctrl = encapsulateCtrls ctrls
          tgate = RotGate tname inv angle tgins tctrl
abstractGate (GPhase angle _ ctrls _) = tgate
    where tctrl = encapsulateCtrls ctrls
          tgate = PhaseGate angle tctrl

-------------------------------------------------------------------------------
-- * Circuit Abstraction.

-- | Helper function to convert Quipper internal wire types to the wire types
-- used within the translator.
arityToWire :: Wiretype -> Quip.Parser.WireType
arityToWire Qbit = QWire
arityToWire Cbit = CWire

-- | Consumes the functional representation of a Quipper circuit (with optional
-- post-processing). Returns a gate-based representation of the circuit. If the
-- conversion fails, then an error is raised.
quipToGates :: QuipCirc -> GateCirc
quipToGates (QuipCirc fn sp) = GateCirc { inputs  = IntMap.map arityToWire ins
                                        , gates   = map abstractGate gates
                                        , outputs = IntMap.map arityToWire outs
                                        , size    = sz
                                        }
    where efn msg                = "quipToGates: encapsulate_generic: " ++ msg
          (_, bcirc, _)          = encapsulate_generic efn fn sp
          (circ, _)              = bcirc
          (ins, gates, outs, sz) = circ

-------------------------------------------------------------------------------
-- * Circuit Parsing.

-- | Consumes the name of an input stream (fp) and the contents of the input
-- stream (input). If input contaisn a valid Quipper circuit in ASCII format,
-- then the Quipper circuit is returned. Otherwise, an error is raised.
parseQuip :: FilePath -> String -> QuipCirc
parseQuip fp input = QuipCirc fn sp
    where (sp, fn) = parse_circuit input
