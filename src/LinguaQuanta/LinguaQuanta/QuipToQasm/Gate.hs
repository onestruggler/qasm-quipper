-- | Functions to translate Quipper gates to OpenQASM gates.

module LinguaQuanta.QuipToQasm.Gate (namedGateTransl) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.AST (AstStmt(..))
import LinguaQuanta.Qasm.Gate
  ( Gate(..)
  , GateMod
  , addCtrlsToMod
  , addNegCtrlsToMod
  , negateMod
  , nullGateMod
  )
import qualified LinguaQuanta.Qasm.GateName as Qasm
import LinguaQuanta.Qasm.Language (GateOperand(..))
import LinguaQuanta.Quip.Gate
  ( Wire
  , Control(..)
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

-- | Takes as input a flag indicating if a gate is inverted, together with a
-- list of Quipper controls. Returns an equivalent OpenQASM 3 gate modifier.
toGateMod :: Bool -> [Control] -> GateMod
toGateMod False []              = nullGateMod
toGateMod True  []              = negateMod nullGateMod
toGateMod inv   ((Pos _):ctrls) = addCtrlsToMod 1 $ toGateMod inv ctrls
toGateMod inv   ((Neg _):ctrls) = addNegCtrlsToMod 1 $ toGateMod inv ctrls

-- | Takes as input a map from Quipper wires to OpenQASM declarations (wrap),
-- a list of Quipper controls (ctrls), and a list of Quipper input wires (ins).
-- Returns an equivalent list of OpenQASM 3 gate operands, such that each wire
-- maps to the corresponding declaration in wmap.
--
-- Note: If a wire is not found is wmap, then an error is raised.
mergeCtrlsAndIns :: WireLookup -> [Control] -> [Wire] -> [GateOperand]
mergeCtrlsAndIns wmap ctrls ins = map f $ toWires ctrls ++ ins
    where f w = case getState w wmap of
                    Nothing    -> error "Undefined control wire."
                    Just CWire -> error "Classical controls are unsupported."
                    Just QWire -> case getAllocation QWire w wmap of
                        Nothing   -> error "Undefined quantum control wire."
                        Just decl -> decl

-------------------------------------------------------------------------------
-- * Named Gate Translation.

-- | Function to a generate a sequence of OpenQASM gates, using the given wires
-- and controls. This is a utility to improve function type readability.
type GateGenerator = [Wire] -> [Control] -> [AstStmt]

-- | Takes as input a map from Quipper wires to OpenQASM declarations (wrap),
-- an OpenQASM gate name, and the description of a Quipper NAmedGate (excluding
-- the name). Returns an equivalent OpenQASM gate with the provided name.
toNamedGateStmt :: WireLookup -> Qasm.GateName -> Bool -> GateGenerator
toNamedGateStmt wmap name inv ins ctrls = [AstGateStmt 1 gate]
    where ops  = mergeCtrlsAndIns wmap ctrls ins
          mods = toGateMod inv ctrls
          gate = NamedGate name [] ops mods

-- | Like toNamedGateStmt, but consumes the first control as an explicit
-- argument. The control is promoted to an input wire. If the polarity of the
-- control is negative, then the negations are inlined.
toNamedCtrl :: WireLookup -> Qasm.GateName -> Bool -> Control -> GateGenerator
toNamedCtrl wmap name inv (Pos w) ins ctrls = stmts
    where stmts = toNamedGateStmt wmap name inv (w:ins) ctrls
toNamedCtrl wmap name inv (Neg w) ins ctrls = stmts
    where invwStmts = namedGateTransl wmap Quip.GateX False [w] []
          gateStmts = toNamedCtrl wmap name inv (Pos w) ins ctrls
          stmts     = invwStmts ++ gateStmts ++ invwStmts

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
-- Unimplemented cases.
namedGateTransl _ _ _ _ _ = error "Unimplemented gate translation case."
