-- | Functions to translate OpenQASM gates to Quipper gates.

module LinguaQuanta.QasmToQuip.Gate
  ( d1RotTransl
  , d2RotTransl
  , d3RotTransl
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

-- | Takes as input the textual name of a gate (for error reporting), a list of
-- input wires (w:ins), a list of controls (ctrls), a Quipper circuit that is
-- parameterized by inputs and controls (f). Generates f with the first input
-- wire promoted to a positive control. Formally, this is: f ins (Pos w:ctrls).
--
-- Note: Requires at least two input wires. Otherwise, an error is raised.
expandCtrl :: String -> [Wire] -> [Control] -> QuipCircFn -> [Gate]
expandCtrl name (w:ins) ctrls f
    | null ins  = error msg
    | otherwise = f ins $ Pos w:ctrls
    where msg = name ++ " requires an additional control operand."
expandCtrl name _ _ _ = error msg
    where msg = name ++ " requires at least two operands."

-- | Takes as input the textual name of a gate (for error reporting), a list of
-- input wires (w1:w2:ins), a list of controls (ctrls), a Quipper circuit that
-- is parameterized by inputs and controls (f). Generates f with the second
-- input wire promoted to a positive controls, i.e.: f (w1:ins) (Pos w2:ctrls).
-- This function is intended for use with expandCtrl, to promote a pair of
-- control wires to controls (e.g., CCX to X with two controls).
--
-- Note: Requires at least two input wires. Otherwise, an error is raised.
expandDblCtrl :: String -> [Wire] -> [Control] -> QuipCircFn -> [Gate]
expandDblCtrl _ (w1:w2:ins) ctrls f = gates
    where gates = f (w1:ins) (Pos w2:ctrls)
expandDblCtrl name _ _ _ = error msg
    where msg = name ++ " requires two control operands."

-------------------------------------------------------------------------------
-- * GPhase Gate Translation.

-- | Implementation details for translGPhase.
toGPhase :: Expr -> Bool -> [Control] -> Gate
toGPhase param inv ctrls =
    case toConstFloat $ Div param Pi of
        Right err -> error "Non-contant float expression as global phase."
        Left pval -> let angle = if inv then -pval else pval
                     in PhaseGate angle ctrls

-- | Takes as input a wire allocation map (wmap), an OpenQASM expression for a
-- compile-time angle (param), a list of operands (ops), and a modifier (mod).
-- Returns a list of Quipper gates quivalent to `mod @ gphase(param) ops` where
-- each operand is mapped to a wire according to wmap. The Quipper gate will
-- have a timestep of `param / PI` to account for the difference in units.
translGPhase :: WireAllocMap -> Expr -> GateGenerator
translGPhase wmap param ops mod = [toGPhase param inv ctrls]
    where wires = opsToWires wmap ops
          inv   = hasInversionMod mod
          ctrls = snd $ extractCtrls wires mod

-------------------------------------------------------------------------------
-- * 3D-Rotation Translation.

-- | Translation details for the U3 gate.
toU3Gate :: Dim3Rot -> Bool -> QuipCircFn
toU3Gate (p1, p2, p3) inv ins ctrls = circ
    where -- Angles
          theta  = if inv then Negate p1 else p1
          phi    = if inv then Negate p3 else p2
          lambda = if inv then Negate p2 else p3
          param2 = Plus phi $ Div Pi $ DecInt "2"
          param3 = Minus lambda $ Div Pi $ DecInt "2"
          -- Global Phase
          phase = toGPhase theta False ctrls
          -- Rotations
          rot1 = toRZGate theta  False ins ctrls
          rot2 = toRZGate param2 False ins ctrls
          rot3 = toRZGate param3 False ins ctrls
          -- Unitary Gates
          omega = toNamedGate Qasm.GateQuipOmega inv   ins ctrls
          gateh = toNamedGate Qasm.GateH         False ins []
          -- Result
          pref = omega ++ omega ++ [phase]
          circ = pref ++ rot1 ++ gateh ++ rot2 ++ gateh ++ rot3

-- | Translation details for the U gate.
toUGate :: Dim3Rot -> Bool -> QuipCircFn
toUGate params@(_, p2, p3) inv ins ctrls = phase : gates
    where -- Angles
          phi    = if inv then Negate p3 else p2
          lambda = if inv then Negate p2 else p3
          -- Global PHase
          angle = Div (Plus phi lambda) $ DecInt "2"
          phase = toGPhase angle False ctrls
          -- Result
          gates = toU3Gate params inv ins ctrls

-- | Same as toRotGate, but for d2RotTransl.
from3DRot :: Qasm.GateName -> Dim3Rot -> Bool -> QuipCircFn
-- Supported 3D-rotations.
from3DRot Qasm.GateU params inv ins ctrls = gates
    where gates = toUGate params inv ins ctrls
from3DRot Qasm.GateU3 params inv ins ctrls = gates
    where gates = toU3Gate params inv ins ctrls
from3DRot Qasm.GateCU param inv ins ctrls = expandCtrl "U3" ins ctrls f
    where f = from3DRot Qasm.GateU param inv
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
toU2Gate (p1, p2) inv ins ctrls = circ
    where -- Angles
          phi    = if inv then Negate p2 else p1
          lambda = if inv then Negate p1 else p2
          param1 = Plus phi $ Div Pi $ DecInt "2"
          param2 = Minus lambda $ Div Pi $ DecInt "2"
          -- Rotations
          rot1 = toRZGate param1 False ins ctrls
          rot2 = toRZGate param2 False ins ctrls
          -- Unitary Gates
          phase = toNamedGate Qasm.GateQuipOmega inv   ins ctrls
          gateh = toNamedGate Qasm.GateH         False ins []
          gates = toNamedGate Qasm.GateS         inv   ins ctrls
          -- Result
          circ = phase ++ rot1 ++ gateh ++ gates ++ gateh ++ rot2

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
    where f = toRotGate name param inv

-- | Translation details for the U1 gate.
toU1Gate :: Dim1Rot -> Bool -> QuipCircFn
toU1Gate param inv ins ctrls = gphase : rzgate
    where phase  = Div param $ DecInt "2"
          gphase = toGPhase phase inv ctrls
          rzgate = toRZGate param inv ins ctrls

-- | Translation details for the P gate.
toPGate :: Dim1Rot -> Bool -> QuipCircFn
toPGate param inv [w] ctrls = [gates]
    where gates = toGPhase param inv $ Pos w:ctrls
toPGate _ _ ins _ = error msg
    where num = show $ length ins
          msg = "P gate requires one non-control input, found: " ++ num

toRZGate :: Dim1Rot -> Bool -> QuipCircFn
toRZGate param inv ins ctrls = 
    case toConstFloat $ Div param $ DecInt "2" of
        Right err -> error "Non-contant float expression as global phase."
        Left tval -> [RotGate Quip.RotExpZ inv tval ins ctrls]

-- | Translation details for the RY gate.
toRXGate :: Dim1Rot -> Bool -> QuipCircFn
toRXGate param inv ins ctrls = circ
    where -- Angles.
          t = Div param $ DecInt "2"
          -- Rotational Gates.
          phase = toGPhase t     inv     ctrls
          zrot  = toRZGate param inv ins ctrls
          -- Unitary Gates
          hgate   = toNamedGate Qasm.GateH False ins []
          -- Result
          circ = [phase] ++ hgate ++ zrot ++ hgate

-- | Translation details for the RY gate.
toRYGate :: Dim1Rot -> Bool -> QuipCircFn
toRYGate param inv ins ctrls = circ
    where -- Rotational Gates.
          phase = toGPhase param inv     ctrls
          zrot  = toRZGate param inv ins ctrls
          -- Unitary Gates
          sgate   = toNamedGate Qasm.GateS False ins []
          hgate   = toNamedGate Qasm.GateH False ins []
          sdggate = toNamedGate Qasm.GateS True  ins []
          -- Result
          circ = [phase] ++ sgate ++ hgate ++ zrot ++ hgate ++ sdggate

-- | Translates a CP gate from OpenQASM to Quipper, with the given input wires
-- and additional controls.
expandCP :: Dim1Rot -> Bool -> QuipCircFn
expandCP param inv ins ctrls = expandDblCtrl "CP" ins ctrls f
    where f = toPGate param inv

-- | Implementation details for d1RotTransl. The Boolaen flag indicates if the
-- gate modifier was inverted. The wires and controls are obtained by first
-- mapping the operands to wires, and then partitioning the wires as either
-- inputs or controls, according to the gate modifier.
toRotGate :: Qasm.GateName -> Dim1Rot -> Bool -> QuipCircFn
-- Translates the only rotation supported in Quipper.
toRotGate Qasm.GateRZ param inv ins ctrls = gates
    where gates = toRZGate param inv ins ctrls
-- Translates rotations with known decompositions.
toRotGate Qasm.GateRX param inv ins ctrls = gates
    where gates = toRXGate param inv ins ctrls
toRotGate Qasm.GateRY param inv ins ctrls = gates
    where gates = toRYGate param inv ins ctrls
-- The P gate is in fact a controlled global phase.
toRotGate Qasm.GateP param inv ins ctrls = gates
    where gates = toPGate param inv ins ctrls
-- The U1 and Phase are equivalent.
toRotGate Qasm.GateU1 param inv ins ctrls = gates
    where gates = toU1Gate param inv ins ctrls
toRotGate Qasm.GatePhase param inv ins ctrls = gates
    where gates = toU1Gate param inv ins ctrls
-- Expands controlled instances.
toRotGate Qasm.GateCRZ param inv ins ctrls = gates
    where gates = expandCRot Qasm.GateRZ param inv ins ctrls
toRotGate Qasm.GateCRX param inv ins ctrls = gates
    where gates = expandCRot Qasm.GateRX param inv ins ctrls
toRotGate Qasm.GateCRY param inv ins ctrls = gates
    where gates = expandCRot Qasm.GateRY param inv ins ctrls
toRotGate Qasm.GateCPhase param inv ins ctrls = gates
    where gates = expandCRot Qasm.GatePhase param inv ins ctrls
-- Expands doubly controlled instances.
toRotGate Qasm.GateCP param inv ins ctrls = gates
    where gates = expandCP param inv ins ctrls
-- Special case: User-defined rotations.
toRotGate (Qasm.UserDefined name) _ _ _ _ = error msg
    where msg = "User-defined gate " ++ name ++ " not implemented."
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
expandToffoli ins ctrls = expandDblCtrl "Toffoli" ins ctrls f
    where f = expandSingleCtrl Quip.GateX

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
toNamedGate (Qasm.UserDefined name) _ _ _ = error msg
    where msg = "User-defined gate " ++ name ++ " not implemented."
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
