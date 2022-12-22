-- | The top-level functions to translate Quipper to OpenQASM.

module QuipToQasm.Translator where

import Passes (applySafePerLinePass)
import Qasm.AST (AstStmt(..))
import Quip.Gate (Gate(..))
import Quip.Parser (GateCirc(..))
import Quip.Wire (countQWires, countCWires)

-- |
translateGate :: Gate -> [AstStmt]
translateGate = error "Gate translation not yet supported."

-- |
translateGates :: [Gate] -> [AstStmt]
translateGates applySafePerLinePass translateGate

-- |
prepareWires :: Int -> Int -> [AstStmt]
prepareWires qcount ccount

-- |
translate :: GateCirc -> [AstStmt]
translate circ = wireTransl ++ bodyTransl
    where qcount     = countQWires $ inputs circ
          ccount     = countCWires $ inputs circ
          wireTransl = prepareWires qcount ccount
          bodyTransl = translateGates $ gates circ
