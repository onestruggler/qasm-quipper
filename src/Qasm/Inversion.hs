-- | Functions to compute inverse gates.

module Qasm.Inversion
  ( negateParams
  , threeParamInversion
  ) where

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

-- | Takes as input a unitary gate g with three parameters, (a, b, c). Returns
-- a new gate with the same name, operands, and modifiers as g, and the
-- parameters (-a, -c, -b). For gates such as U and CU, this function computes
-- the inverse operation to g. If G does not have three parameters, then
-- nothing is returned.
threeParamInversion :: Gate -> Maybe Gate
threeParamInversion (NamedGate name [a, b, c] operands mod) = Just result
    where params = [negateExpr a, negateExpr c, negateExpr b]
          result = NamedGate name params operands mod
threeParamInversion _ = Nothing
