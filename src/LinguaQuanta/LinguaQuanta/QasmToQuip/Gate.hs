-- | Functions to translate OpenQASM gates to Quipper gates.

module LinguaQuanta.QasmToQuip.Gate
  ( d1RotTransl
  , namedGateTransl
  , translGPhase
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.Expression (toConstFloat)
import LinguaQuanta.Qasm.Gate
  ( GateMod
  , Operand(..)
  , hasInversionMod
  )
import qualified LinguaQuanta.Qasm.GateName as Qasm
import LinguaQuanta.Qasm.Language (Expr(..))
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

-- | Helper to describe the parameters to a 1-dimensional OpenQASM rotation.
type Dim1Rot = Expr

-- | Helper to describe the parameters to a 2-dimensional OpenQASM rotation.
type Dim2Rot = (Expr, Expr)

-- | Helper to describe the parameters to a 3-dimensional OpenQASM rotation.
type Dim3Rot = (Expr, Expr, Expr)

-- | Function to a generate a sequence of Quipper, using the given operands and
-- modifiers. This is a utility to improve function type readability.
type GateGenerator = [Operand] -> GateMod -> [Gate]

-- |
type QuipCircFn = [Wire] -> [Control] -> [Gate]

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

-- |
expandCtrl :: String -> [Wire] -> [Control] -> QuipCircFn -> [Gate]
expandCtrl name (w:ins) ctrls f
    | null ins  = error msg
    | otherwise = f ins $ Pos w:ctrls
    where msg = name ++ " requires an additional control operand."
expandCtrl name _ _ _ = error msg
    where msg = name ++ " requires at least two operands."

-------------------------------------------------------------------------------
-- * GPhase Gate Translation.

-- | Takes as input a wire allocation map (wmap), an OpenQASM expression for a
-- compile-time angle (param), a list of operands (ops), and a modifier (mod).
-- Returns a list of Quipper gates quivalent to `mod @ gphase(param) ops` where
-- each operand is mapped to a wire according to wmap. The Quipper gate will
-- have a timestep of `param / PI` to account for the difference in units.
translGPhase :: WireAllocMap -> Expr -> GateGenerator
translGPhase wmap param ops mod =
    case toConstFloat $ Div param Pi of
        Right err -> error "Non-contant float expression as global phase."
        Left pval -> let angle = if hasInversionMod mod then -pval else pval
                     in [PhaseGate angle ctrls] 
    where wires = opsToWires wmap ops
          ctrls = snd $ extractCtrls wires mod

-------------------------------------------------------------------------------
-- * 3D-Rotation Translation.

-- | Same as toRotGate, but for d2RotTransl.
from3DRot :: Qasm.GateName -> Dim3Rot -> Bool -> QuipCircFn
-- Supported 3D-rotations.
from3DRot Qasm.GateU param inv ins ctrls = error msg
    where msg = "Translation not implemented for: U."
from3DRot Qasm.GateU3 param inv ins ctrls = error msg
    where msg = "Translation not implemented for: U3."
from3DRot Qasm.GateCU param inv ins ctrls = expandCtrl "U3" ins ctrls f
    where f x y = from3DRot Qasm.GateU param inv x y
-- Named 3D-rotations are not supported.
from3DRot (Qasm.UserDefined _) _ _ _ _ = error msg
    where msg = "User-defined rotations must be one-dimensional (not 3D)."
-- Error case.
from3DRot _ name _ _ _ = error msg
    where msg = "Unexpected RotGate in OpenQASM: " ++ show name

-- | Same as d1RotTransl, but for Dim3Rot parameters.
d3RotTransl :: WireAllocMap -> Qasm.GateName -> Dim3Rot -> GateGenerator
d3RotTransl wmap name param ops mod = from3DRot name param inv ins ctrls
    where wires        = opsToWires wmap ops
          inv          = hasInversionMod mod
          (ins, ctrls) = extractCtrls wires mod

-------------------------------------------------------------------------------
-- * 2D-Rotation Translation.

-- | Same as toRotGate, but for d2RotTransl.
toU2Gate :: Dim2Rot -> Bool -> QuipCircFn
toU2Gate (p1, p2) inv ins ctrls = error msg
    where msg = "Translation not implemented for: U2."

-- | Same as d2RotTransl, but for Dim2Rot parameters.
d2RotTransl :: WireAllocMap -> Qasm.GateName -> Dim2Rot -> GateGenerator
-- Supported 2D-rotations.
d2RotTransl wmap Qasm.GateU2 param ops mod = toU2Gate param inv ins ctrls
    where wires        = opsToWires wmap ops
          inv          = hasInversionMod mod
          (ins, ctrls) = extractCtrls wires mod
-- Named 2D-rotations are not supported.
d2RotTransl _ (Qasm.UserDefined _) _ _ _ = error msg
    where msg = "User-defined rotations must be one-dimensional (not 2D)."
-- Error case.
d2RotTransl _ name _ _ _ = error msg
    where msg = "Unexpected RotGate in OpenQASM: " ++ show name

-------------------------------------------------------------------------------
-- * 1D-Rotation Translation.

-- | Translates a gate of the form C(name(param)) from OpenQASM to Quipper, with
-- the given input wires and additional controls.
expandCRot :: Qasm.GateName -> Dim1Rot -> Bool -> QuipCircFn
expandCRot name param inv ins ctrls = expandCtrl (show name) ins ctrls f
    where f x y = toRotGate name param inv x y

-- | Implementation details for d1RotTransl. The Boolaen flag indicates if the
-- gate modifier was inverted. The wires and controls are obtained by first
-- mapping the operands to wires, and then partitioning the wires as either
-- inputs or controls, according to the gate modifier.
toRotGate :: Qasm.GateName -> Dim1Rot -> Bool -> QuipCircFn
-- Translates the only rotation supported in Quipper.
toRotGate Qasm.GateRZ param inv ins ctrls =
    case toConstFloat $ Div param $ DecInt "2" of
        Right err -> error "Non-contant float expression as global phase."
        Left tval -> [RotGate Quip.RotExpZ inv tval ins ctrls]
-- Translates rotations with known decompositions.
toRotGate Qasm.GateRX param inv ins ctrls = error msg
    where msg = "Translation not implemented for: RX."
toRotGate Qasm.GateRY param inv ins ctrls = error msg
    where msg = "Translation not implemented for: RY."
-- The P gate is in fact a controlled global phase.
toRotGate Qasm.GateP param inv ins ctrls = error msg
    where msg = "Translation not implemented for: P."
-- The U1 and Phase are equivalent.
toRotGate Qasm.GateU1 param inv ins ctrls = error msg
    where msg = "Translation not implemented for: U1."
toRotGate Qasm.GatePhase param inv ins ctrls = gates
    where gates = toRotGate Qasm.GateU1 param inv ins ctrls
-- Expands controlled instances.
toRotGate Qasm.GateCRZ param inv ins ctrls = gates
    where gates = expandCRot Qasm.GateRZ param inv ins ctrls
toRotGate Qasm.GateCRX param inv ins ctrls = gates
    where gates = expandCRot Qasm.GateRX param inv ins ctrls
toRotGate Qasm.GateCRY param inv ins ctrls = gates
    where gates = expandCRot Qasm.GateRY param inv ins ctrls
toRotGate Qasm.GateCP param inv ins ctrls = gates
    where gates = expandCRot Qasm.GateP param inv ins ctrls
toRotGate Qasm.GateCPhase param inv ins ctrls = gates
    where gates = expandCRot Qasm.GatePhase param inv ins ctrls
-- Special case: User-defined rotations.
toRotGate (Qasm.UserDefined _) _ _ _ _ = error msg
    where msg = "User-defined gate translations not implemented."
-- Error case.
toRotGate name _ _ _ _ = error msg
    where msg = "Unexpected RotGate in OpenQASM: " ++ show name

-- | Takes as input a wire allocation map (wmap), the name of a 1D-rotation
-- gate, an OpenQASM expression for a compile-time angle (param), a list of
-- operands (ops), and a modifier (mod). Returns a list of Quipper gates
-- quivalent to `mod @ name(param) ops` where each operand is mapped to a wire
-- according to wmap.
d1RotTransl :: WireAllocMap -> Qasm.GateName -> Dim1Rot -> GateGenerator
d1RotTransl wmap name param ops mod = toRotGate name param inv ins ctrls
    where wires        = opsToWires wmap ops
          inv          = hasInversionMod mod
          (ins, ctrls) = extractCtrls wires mod

-------------------------------------------------------------------------------
-- * (Zero-Parameter) Named Gate Translation.

-- | Translates a gate of the form C(name) from OpenQASM to Quipper, with the
-- given input wires and additional controls.
expandSingleCtrl :: Quip.GateName -> QuipCircFn
expandSingleCtrl name ins ctrls = expandCtrl (show name) ins ctrls f
    where f x y = [NamedGate name False x y]

-- | Translates a CCX gate from OpenQASM to Quipper, with the given input wires
-- and additional controls.
expandToffoli :: QuipCircFn
expandToffoli (w1:w2:ins) ctrls = gates
    where gates = expandSingleCtrl Quip.GateX (w1:ins) (Pos w2:ctrls)
expandToffoli name _ = error "Toffoli requires two control operands."

-- | Implementation details for namedGateTransl. The Boolaen flag indicates if
-- the gate modifier was inverted. The wires and controls are obtained by first
-- mapping the operands to wires, and then partitioning the wires as either
-- inputs or controls, according to the gate modifier.
toNamedGate :: Qasm.GateName -> Bool -> QuipCircFn
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
