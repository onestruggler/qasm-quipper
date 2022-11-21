-- | Functions to compute inverse gates.

module Qasm.Inversion (negateParams) where

import Qasm.Expression (negateExpr)
import Qasm.Gate (Gate(..))

-------------------------------------------------------------------------------
-- * Inversion Schemes.

-- | Negates all parameters provided to a gate.
negateParams :: Gate -> Gate
negateParams (NamedGate name params operands mod)
    = NamedGate name (map negateExpr params) operands mod
negateParams (GPhaseGate param operands mod)
    = GPhaseGate (negateExpr param) operands mod
