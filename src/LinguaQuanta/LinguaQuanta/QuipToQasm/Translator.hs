-- | The top-level functions to translate Quipper to OpenQASM.

module LinguaQuanta.QuipToQasm.Translator (translate) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.AST (AstStmt(..))
import LinguaQuanta.Quip.Gate (Gate(..))
import LinguaQuanta.Quip.Quipper (GateCirc(..))
import LinguaQuanta.Quip.Wire (countQWires, countCWires)
import LinguaQuanta.QuipToQasm.Gate
  ( namedGateTransl
  , translGPhase
  )
import LinguaQuanta.QuipToQasm.Wire
  ( WireLookup
  , allocateInputWires
  , translateQWireInputs
  , translateCWireInputs
  )

-------------------------------------------------------------------------------
-- * Functions to Translate Gates

-- | Takes as input a Quipper gate. Returns an equivalent sequence of OpenQASM
-- statements (given by a list of AST statements).
translateGate :: WireLookup -> Gate -> [AstStmt]
translateGate wmap (NamedGate name inv ins ctrls) = stmts
    where stmts = namedGateTransl wmap name inv ins ctrls
translateGate wmap (RotGate name inv angle ins ctrls) = error msg
    where msg = "RotGate translation not implemented."
translateGate wmap (PhaseGate t ctrls) = stmts
    where stmts = translGPhase wmap t ctrls
translateGate wmap (QInitGate isOn id) = error msg
    where msg = "QInitGate translation not implemented."
translateGate wmap (QTermGate isOn id) = error msg
    where msg = "QTermGate translation not implemented."

-- | Takes as input the body of a Quipper circuit or subroutine (given by a
-- list of gates). Returns the body of an equivalent OpenQASM program (given by
-- a list of AST statements).
translateGates :: WireLookup -> [Gate] -> [AstStmt]
translateGates _    []          = []
translateGates wmap (gate:rest) = gateStmts ++ restStmts
    where gateStmts = translateGate wmap gate
          restStmts = translateGates wmap rest

-------------------------------------------------------------------------------
-- * Top-Level Translation Functions

-- | Consumes a Quipper circuit. Returns an equivalent OpenQASM circuit.
translate :: GateCirc -> [AstStmt]
translate circ = qwires ++ cwires ++ body
    where qwires = translateQWireInputs $ countQWires $ inputs circ
          cwires = translateCWireInputs $ countCWires $ inputs circ
          allocs = allocateInputWires $ inputs circ
          body   = translateGates allocs $ gates circ
