-- | The top-level functions to translate Quipper to OpenQASM.

module LinguaQuanta.QuipToQasm.Translator (translate) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.AST (AstStmt(..))
import LinguaQuanta.Quip.Gate (Gate(..))
import LinguaQuanta.Quip.Quipper (GateCirc(..))
import LinguaQuanta.Quip.Wire (countQWires, countCWires)

-------------------------------------------------------------------------------
-- * Functions to Translate Gates

-- | Consumes the body of a Quipper circuit or subroutine (a list of gates).
-- Returns the body of an equivalent OpenQASM program (a list of statements).
translateGates :: [Gate] -> [AstStmt]
translateGates [] = []
translateGates _  = error "Gate translation not yet supported."

-------------------------------------------------------------------------------
-- * Functions to Translate Inputs

-- | The global register for all input qubits.
_IN_QWIRE_REG :: String
_IN_QWIRE_REG = "input_qwires"

-- | The global register for all input classical bits.
_IN_CWIRE_REG :: String
_IN_CWIRE_REG = "input_cwires"

-- | Consumes the number of input qubits. Returns a list of AST statements to
-- declare the corresponding register.
translateQWireInputs :: Int -> [AstStmt]
translateQWireInputs 0 = []
translateQWireInputs n = [AstQubitDecl (Just n) _IN_QWIRE_REG]

-- | Consumes the number of input classical bits. Returns a list of AST
-- statements to declare the corresponding register.
translateCWireInputs :: Int -> [AstStmt]
translateCWireInputs 0 = []
translateCWireInputs _ = error "Classical wires not yet supported."

-------------------------------------------------------------------------------
-- * Top-Level Translation Functions

-- | Consumes a Quipper circuit. Returns an equivalent OpenQASM circuit.
translate :: GateCirc -> [AstStmt]
translate circ = qwires ++ cwires ++ body
    where qwires = translateQWireInputs $ countQWires $ inputs circ
          cwires = translateCWireInputs $ countCWires $ inputs circ
          body   = translateGates $ gates circ
