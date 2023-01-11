-- | Functions to translate OpenQASM gates to Quipper gates.

module LinguaQuanta.QasmToQuip.Gate (namedGateTransl) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.Expression (toConstInt)
import LinguaQuanta.Qasm.Gate
  ( GateMod
  , Operand(..)
  , hasInversionMod
  )
import qualified LinguaQuanta.Qasm.GateName as Qasm
import LinguaQuanta.QasmToQuip.Control (extractCtrls)
import LinguaQuanta.QasmToQuip.Wire
  ( WireAllocMap
  , getCellIndex
  , getScalarIndex
  )
import LinguaQuanta.Quip.Gate
  ( Control(..)
  , Gate(..)
  , Wire
  )
import qualified LinguaQuanta.Quip.GateName as Quip

-------------------------------------------------------------------------------
-- * Gate Translation Utilities.

-- | Function to a generate a sequence of Quipper, using the given operands and
-- modifiers. This is a utility to improve function type readability.
type GateGenerator = [Operand] -> GateMod -> [Gate]

-- | Takes as input a wire allocation map (wmap) and an operand (op). If the
-- declaration for op is recorded in wmap, then the corresponding wire index is
-- returned. Otherwise, and error is raised.
opToWire :: WireAllocMap -> Operand -> Wire
opToWire wmap (QRef id) =
    case getScalarIndex id wmap of
        Nothing -> error $ "Unknown qvar used as operand: " ++ id
        Just w  -> w
opToWire wmap (Cell id idx) =
    case getCellIndex id idx wmap of
        Nothing -> error $ "Unknown qreg used as operand: [" ++ id ++ "]"
        Just w  -> w

-- | Takes as input a wire allocation map (wmap) and an operand (ops). Returns
-- a list of wires, computed as follows. For each op in ops, the declaration
-- for op is retrieved from wmap, and then the corresponding wire is added to
-- the output map in order.
--
-- Note: If a declaration does not appear in wmap, then an error is raised.
opsToWires :: WireAllocMap -> [Operand] -> [Wire]
opsToWires wmap = map (opToWire wmap)

-------------------------------------------------------------------------------
-- * (Zero-Parameter) Named Gate Translation.

-- | Translates a gate of the form C(name) from OpenQASM to Quipper, with the
-- given input wires and additional controls.
expandSingleCtrl :: Quip.GateName -> [Wire] -> [Control] -> [Gate]
expandSingleCtrl name (w:ins) ctrls = [gate]
    where gate = NamedGate name False ins (Pos w:ctrls)
expandSingleCtrl name _ _ = error msg
    where msg = show name ++ " requires an additional control operand."

-- | Translates a CCX gate from OpenQASM to Quipper, with the given input wires
-- and additional controls.
expandToffoli :: [Wire] -> [Control] -> [Gate]
expandToffoli (w1:w2:ins) ctrls = gates
    where gates = expandSingleCtrl Quip.GateX (w1:ins) (Pos w2:ctrls)
expandToffoli name _ = error "Toffoli requires two control operands."

-- | Implementation details for namedGateTransl. The Boolaen flag indicates if
-- the gate modifier was inverted. The wires and controls are obtained by first
-- mapping the operands to wires, and then partitioning the wires as either
-- inputs or controls, according to the gate modifier.
toNamedGate :: Qasm.GateName -> Bool -> [Wire] -> [Control] -> [Gate]
-- Translations for gates that are not controlled instances.
toNamedGate Qasm.GateX _ ins ctrls = [gate]
    where gate = NamedGate Quip.GateX False ins ctrls
toNamedGate Qasm.GateY _ ins ctrls = [gate]
    where gate = NamedGate Quip.GateY False ins ctrls
toNamedGate Qasm.GateZ _ ins ctrls = [gate]
    where gate = NamedGate Quip.GateZ False ins ctrls
toNamedGate Qasm.GateH _ ins ctrls = [gate]
    where gate = NamedGate Quip.GateH False ins ctrls
toNamedGate Qasm.GateSwap _ ins ctrls = [gate]
    where gate = NamedGate Quip.GateSwap False ins ctrls
toNamedGate Qasm.GateS inv ins ctrls = [gate]
    where gate = NamedGate Quip.GateS inv ins ctrls
toNamedGate Qasm.GateSX inv ins ctrls = [gate]
    where gate = NamedGate Quip.GateV inv ins ctrls
toNamedGate Qasm.GateT inv ins ctrls = [gate]
    where gate = NamedGate Quip.GateT inv ins ctrls
toNamedGate Qasm.GateQuipIX inv ins ctrls = [gate]
    where gate = NamedGate Quip.GateIX inv ins ctrls
toNamedGate Qasm.GateQuipOmega inv ins ctrls = [gate]
    where gate = NamedGate Quip.GateOmega inv ins ctrls
toNamedGate Qasm.GateQuipE inv ins ctrls = [gate]
    where gate = NamedGate Quip.GateE inv ins ctrls
toNamedGate Qasm.GateQuipW inv ins ctrls = [gate]
    where gate = NamedGate Quip.GateW inv ins ctrls
-- Translations for gates with implicit inversions.
toNamedGate Qasm.GateSdg inv ins ctrls = gates
    where gates = toNamedGate Qasm.GateS (not inv) ins ctrls
toNamedGate Qasm.GateTdg inv ins ctrls = gates
    where gates = toNamedGate Qasm.GateT (not inv) ins ctrls
-- Translations for gates that are singly controlled instances.
toNamedGate Qasm.GateCX _ ins ctrls = gates
    where gates = expandSingleCtrl Quip.GateX ins ctrls
toNamedGate Qasm.GateCY _ ins ctrls = gates
    where gates = expandSingleCtrl Quip.GateY ins ctrls
toNamedGate Qasm.GateCZ _ ins ctrls = gates
    where gates = expandSingleCtrl Quip.GateZ ins ctrls
toNamedGate Qasm.GateCH _ ins ctrls = gates
    where gates = expandSingleCtrl Quip.GateH ins ctrls
toNamedGate Qasm.GateCSwap _ ins ctrls = gates
    where gates = expandSingleCtrl Quip.GateSwap ins ctrls
-- Special case: Toffoli gate.
toNamedGate Qasm.GateCCX _ ins ctrls = expandToffoli ins ctrls
-- Special case: User-defined gate.
toNamedGate (Qasm.UserDefined _) _ _ _ = error msg
    where msg = "User-defined gate translations not implemented."
-- Error case.
toNamedGate _ name _ _ = error msg
    where msg = "Unexpected NamedGate in OpenQASM: " ++ show name

-- | Takes as input a wire allocation map (wmap), the name of a non-rotational
-- gate (name), a list of operands (ops), and a modifier (mod). Returns a list
-- of Quipper gates equivelent to `mod @ name ops` where each operand is mapped
-- to a wire according to wmap.
namedGateTransl :: WireAllocMap -> Qasm.GateName -> GateGenerator
-- Eliminates ID gates.
namedGateTransl _ Qasm.GateID _ _ = []
-- Separates operands and controls for all other gates.
namedGateTransl wmap name ops mod = toNamedGate name inv ins ctrls
    where wires        = opsToWires wmap ops
          inv          = hasInversionMod mod
          (ins, ctrls) = extractCtrls wires mod
