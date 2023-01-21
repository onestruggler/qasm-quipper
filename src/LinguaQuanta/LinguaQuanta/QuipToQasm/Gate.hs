-- | Functions to translate Quipper gates to OpenQASM gates.

module LinguaQuanta.QuipToQasm.Gate
  ( namedGateTransl
  , translGPhase
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.AST (AstStmt(..))
import LinguaQuanta.Qasm.Gate
  ( Gate(..)
  , GateMod
  , Operand
  , addCtrlsToMod
  , addNegCtrlsToMod
  , negateMod
  , nullGateMod
  )
import qualified LinguaQuanta.Qasm.GateName as Qasm
import LinguaQuanta.Qasm.Language (Expr(DecFloat, Pi, Times))
import LinguaQuanta.Quip.Gate
  ( Control(..)
  , Wire
  , toWires
  )
import qualified LinguaQuanta.Quip.GateName as Quip
import LinguaQuanta.Quip.Wire (WireType(..))
import LinguaQuanta.QuipToQasm.Wire
  ( WireLookup
  , getAllocation
  , getState
  )

-------------------------------------------------------------------------------
-- * Gate Translation Utilities.

-- | Function to a generate a sequence of OpenQASM gates, using the given wires
-- and controls. This is a utility to improve function type readability.
type GateGenerator = [Wire] -> [Control] -> [AstStmt]

-- | Takes as input a flag indicating if a gate is inverted, together with a
-- list of Quipper controls. Returns an equivalent OpenQASM 3 gate modifier.
toGateMod :: Bool -> [Control] -> GateMod
toGateMod False []              = nullGateMod
toGateMod True  []              = negateMod nullGateMod
toGateMod inv   ((Pos _):ctrls) = addCtrlsToMod 1 $ toGateMod inv ctrls
toGateMod inv   ((Neg _):ctrls) = addNegCtrlsToMod 1 $ toGateMod inv ctrls

-- | Takes as input a map from Quipper wires to OpenQASM declarations (wmap),
-- a list of Quipper controls (ctrls), and a list of Quipper input wires (ins).
-- Returns an equivalent list of OpenQASM 3 gate operands, such that each wire
-- maps to the corresponding declaration in wmap.
--
-- Note: If a wire is not found is wmap, then an error is raised.
mergeCtrlsAndIns :: WireLookup -> [Control] -> [Wire] -> [Operand]
mergeCtrlsAndIns wmap ctrls ins = map f $ toWires ctrls ++ ins
    where f w = case getState w wmap of
                    Nothing    -> error "Undefined control wire."
                    Just CWire -> error "Classical controls are unsupported."
                    Just QWire -> case getAllocation QWire w wmap of
                        Nothing   -> error "Undefined quantum control wire."
                        Just decl -> decl

-- | Takes as input a map from Quipper wires to OpenQASM declarations (wmap), a
-- wire (w), and a list of OpenQASM statements (stmts). Returns stmts
-- conjugated by: X w.
conjugateByNots :: WireLookup -> Wire -> [AstStmt] -> [AstStmt]
conjugateByNots wmap w stmts = notStmts ++ stmts ++ notStmts
    where notStmts = namedGateTransl wmap Quip.GateX False [w] []

-------------------------------------------------------------------------------
-- * Phase Gate Translation.

-- | Takes as input a map from Quipper wires to OpenQASM declarations (wmap),
-- an OpenQASM gate name, a rotation expression (param), a designate control,
-- a list of input wires, and a list of controls. Returns the specified named
-- gate with the provided parameter, and the desigated control promoted to an
-- input wire. If the polarity of the control is negative, then the negations
-- are inlined.
toGPhaseCtrl :: WireLookup -> Qasm.GateName -> Expr -> Control -> GateGenerator
toGPhaseCtrl wmap name param (Pos w) ins ctrls = [AstGateStmt 0 gate]
    where ops  = mergeCtrlsAndIns wmap ctrls (w:ins)
          mods = toGateMod False ctrls
          gate = NamedGate name [param] ops mods
toGPhaseCtrl wmap name param (Neg w) ins ctrls = conjugateByNots wmap w stmts
    where stmts = toGPhaseCtrl wmap name param (Pos w) ins ctrls

-- | Like toGPhaseCtrl, but consumes the first two controls and is specialized
-- to the CP gate. Each control is promoted as described by toGPhaseCtrl.
translCP :: WireLookup -> Expr -> Control -> Control -> [Control] -> [AstStmt]
translCP wmap param c (Pos w) ctrls = stmts
    where stmts = toGPhaseCtrl wmap Qasm.GateCP param c [w] ctrls
translCP wmap param c (Neg w) ctrls = conjugateByNots wmap w stmts
    where stmts = translCP wmap param c (Pos w) ctrls

-- | Implementation details for translGPhase. Takes as input a map from Quipper
-- wires to OpenQASM declarations (wmap), a rotation expression (param), and
-- the description of a Quipper gate. Determines whether the global phase gate
-- has zero controls (and returns a gphase gate), one control (and returns a P
-- gate), or multiple controls (and returns a CP gate).
translGPhaseImpl :: WireLookup -> Expr -> [Control] -> [AstStmt]
translGPhaseImpl wmap param [] = [AstGateStmt 0 gate]
    where gate = GPhaseGate param [] nullGateMod
translGPhaseImpl wmap param [c] = stmts
    where stmts = toGPhaseCtrl wmap Qasm.GateP param c [] []
translGPhaseImpl wmap param (c1:c2:ctrls) = stmts
    where stmts = translCP wmap param c1 c2 ctrls

-- | Takes as input a map from Quipper wires to OpenQASM declarations (wmap),
-- a duration (t), and the description of a Quipper gate. Returns an OpenQASM
-- GPhase gate at an angle of (pi*t) with an equivalent list of controls.
translGPhase :: WireLookup -> Double -> [Control] -> [AstStmt]
translGPhase wmap t ctrls = translGPhaseImpl wmap param ctrls
    where param = Times Pi $ DecFloat $ show t

-------------------------------------------------------------------------------
-- * Named Gate Translation.

-- | Takes as input a map from Quipper wires to OpenQASM declarations (wmap),
-- an OpenQASM gate name, and the description of a Quipper NamedGate (excluding
-- the name). Returns an equivalent OpenQASM gate with the provided name.
toNamedGateStmt :: WireLookup -> Qasm.GateName -> Bool -> GateGenerator
toNamedGateStmt wmap name inv ins ctrls = [AstGateStmt 0 gate]
    where ops  = mergeCtrlsAndIns wmap ctrls ins
          mods = toGateMod inv ctrls
          gate = NamedGate name [] ops mods

-- | Like toNamedGateStmt, but consumes the first control as an explicit
-- argument. The control is promoted to an input wire. If the polarity of the
-- control is negative, then the negations are inlined.
toNamedCtrl :: WireLookup -> Qasm.GateName -> Bool -> Control -> GateGenerator
toNamedCtrl wmap name inv (Pos w) ins ctrls = stmts
    where stmts = toNamedGateStmt wmap name inv (w:ins) ctrls
toNamedCtrl wmap name inv (Neg w) ins ctrls = conjugateByNots wmap w stmts
    where stmts = toNamedCtrl wmap name inv (Pos w) ins ctrls

-- | Like toNamedCtrl, but consumes the first two controls and is specialized
-- to the Pauli-X gate. Each control is promoted as described by toNamedCtrl.
translToffoli :: WireLookup -> Control -> Control -> GateGenerator
translToffoli wmap c (Pos w) ins ctrls = stmts
    where stmts = toNamedCtrl wmap Qasm.GateCCX False c (w:ins) ctrls
translToffoli wmap c (Neg w) ins ctrls = conjugateByNots wmap w stmts
    where stmts = translToffoli wmap c (Pos w) ins ctrls

-- | Takes as input a map from Quipper wires to OpenQASM declarations (wmap),
-- a list of n input wires, and a list of controls. The wires and controls are
-- inputs to a MultiQNot. Implements the controlled QMultiNot gate using n
-- controlled Paul-X gates. Returns the Pauli-X gates as AST statements.
unfoldQMultiNot :: WireLookup -> GateGenerator
unfoldQMultiNot wmap []      ctrls = []
unfoldQMultiNot wmap (q:ins) ctrls = gate ++ rest
    where gate = namedGateTransl wmap Quip.GateX False [q] ctrls
          rest = unfoldQMultiNot wmap ins ctrls

-- | Takes as input a map from Quipper wires to OpenQASM declarations (wmap),
-- together with the description of a Quipper NamedGate (name, inv, in, ctrls).
-- Returns a list of equivalent OpenQASM statements.
namedGateTransl :: WireLookup -> Quip.GateName -> Bool -> GateGenerator
-- Uncontrolled gates with named controlled instances.
namedGateTransl wmap Quip.GateX _ ins [] = stmts
    where stmts = toNamedGateStmt wmap Qasm.GateX False ins []
namedGateTransl wmap Quip.GateY _ ins [] = stmts
    where stmts = toNamedGateStmt wmap Qasm.GateY False ins []
namedGateTransl wmap Quip.GateZ _ ins [] = stmts
    where stmts = toNamedGateStmt wmap Qasm.GateZ False ins []
namedGateTransl wmap Quip.GateH _ ins [] = stmts
    where stmts = toNamedGateStmt wmap Qasm.GateH False ins []
namedGateTransl wmap Quip.GateSwap _ ins [] = stmts
    where stmts = toNamedGateStmt wmap Qasm.GateSwap False ins []
-- Controlled gates with named singly controlled instances.
namedGateTransl wmap Quip.GateX _ ins [c] = stmts
    where stmts = toNamedCtrl wmap Qasm.GateCX False c ins []
namedGateTransl wmap Quip.GateY _ ins (c:ctrls) = stmts
    where stmts = toNamedCtrl wmap Qasm.GateCY False c ins ctrls
namedGateTransl wmap Quip.GateZ _ ins (c:ctrls) = stmts
    where stmts = toNamedCtrl wmap Qasm.GateCZ False c ins ctrls
namedGateTransl wmap Quip.GateH _ ins (c:ctrls) = stmts
    where stmts = toNamedCtrl wmap Qasm.GateCH False c ins ctrls
namedGateTransl wmap Quip.GateSwap _ ins (c:ctrls) = stmts
    where stmts = toNamedCtrl wmap Qasm.GateCSwap False c ins ctrls
-- Built-in gates without named controlled instances.
namedGateTransl wmap Quip.GateV inv ins ctrls = stmts
    where stmts = toNamedGateStmt wmap Qasm.GateSX inv ins ctrls
namedGateTransl wmap Quip.GateS inv ins ctrls = stmts
    where stmts = toNamedGateStmt wmap Qasm.GateS inv ins ctrls
namedGateTransl wmap Quip.GateT inv ins ctrls = stmts
    where stmts = toNamedGateStmt wmap Qasm.GateT inv ins ctrls
namedGateTransl wmap Quip.GateIX inv ins ctrls = stmts
    where stmts = toNamedGateStmt wmap Qasm.GateQuipIX inv ins ctrls
namedGateTransl wmap Quip.GateOmega inv ins ctrls = stmts
    where stmts = toNamedGateStmt wmap Qasm.GateQuipOmega inv ins ctrls
namedGateTransl wmap Quip.GateW inv ins ctrls = stmts
    where stmts = toNamedGateStmt wmap Qasm.GateQuipW inv ins ctrls
namedGateTransl wmap Quip.GateE inv ins ctrls = stmts
    where stmts = toNamedGateStmt wmap Qasm.GateQuipE inv ins ctrls
-- Special case: Toffoli gate.
namedGateTransl wmap Quip.GateX _ ins (c1:c2:ctrls) = stmts
    where stmts = translToffoli wmap c1 c2 ins ctrls
-- Special case: QMultiNot gate.
namedGateTransl wmap Quip.GateQMultiNot _ ins ctrls = transl
    where transl = unfoldQMultiNot wmap ins ctrls
-- Special case: User-defined gate.
namedGateTransl _ (Quip.UserDefinedGate _) _ _ _ = error msg
    where msg = "User-defined gate translations not implemented."
