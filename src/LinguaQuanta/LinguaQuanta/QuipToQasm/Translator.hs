-- | The top-level functions to translate Quipper to OpenQASM.

module LinguaQuanta.QuipToQasm.Translator (translate) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.AST (AstStmt(..))
import LinguaQuanta.Quip.Gate (Gate(..))
import LinguaQuanta.Quip.Quipper (GateCirc(..))
import LinguaQuanta.Quip.Wire (countQWires, countCWires)
import LinguaQuanta.QuipToQasm.Wire
  ( translateQWireInputs
  , translateCWireInputs
  )

-------------------------------------------------------------------------------
-- * Functions to Translate Gates

-- | Takes as input a Quipper gate. Returns an equivalent sequence of OpenQASM
-- statements (given by a list of AST statements).
translateGate :: Gate -> [AstStmt]
translateGate (NamedGate name inv ins ctrls) = error msg
    where msg = "NamedGate translation not implemented."
translateGate (RotGate name inv angle ins ctrls) = error msg
    where msg = "RotGate translation not implemented."
translateGate (PhaseGate angle ctrls) = error msg
    where msg = "PhaseGate translation not implemented."
translateGate (QInitGate isOn id) = error msg
    where msg = "QInitGate translation not implemented."
translateGate (QTermGate isOn id) = error msg
    where msg = "QTermGate translation not implemented."

-- | Takes as input the body of a Quipper circuit or subroutine (given by a
-- list of gates). Returns the body of an equivalent OpenQASM program (given by
-- a list of AST statements).
translateGates :: [Gate] -> [AstStmt]
translateGates []          = []
translateGates (gate:rest) = translateGate gate ++ translateGates rest

-------------------------------------------------------------------------------
-- * Top-Level Translation Functions

-- | Consumes a Quipper circuit. Returns an equivalent OpenQASM circuit.
translate :: GateCirc -> [AstStmt]
translate circ = qwires ++ cwires ++ body
    where qwires = translateQWireInputs $ countQWires $ inputs circ
          cwires = translateCWireInputs $ countCWires $ inputs circ
          body   = translateGates $ gates circ
