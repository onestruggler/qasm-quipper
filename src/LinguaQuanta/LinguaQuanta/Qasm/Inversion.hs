-- | Functions to compute inverse gates.

module LinguaQuanta.Qasm.Inversion
  ( invertGate
  , negateParams
  , threeParamInversion
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Maybe
  ( maybeAppend
  , maybeWrap 
  )
import LinguaQuanta.Qasm.Expression
  ( avgExpr
  , negateExpr
  , zero
  )
import LinguaQuanta.Qasm.Gate
  ( Gate(..)
  , GateMod
  , Operand
  , invert
  , isInverted
  )
import LinguaQuanta.Qasm.GateName
  ( GateName(..)
  , isParamInverse
  , isSelfInverse
  , toOperandCount
  )
import LinguaQuanta.Qasm.Language (Expr(..))

-------------------------------------------------------------------------------
-- * Useful Constants.

-- | The global phase of omega inverse.
sevenFourthsPi = Times (Div (DecInt "7") (DecInt "4")) Pi

-- | The global phase of omega^3 inverse.
fiveFourthsPi = Times (Div (DecInt "5") (DecInt "4")) Pi

-- | Appears in the inverse of u2.
halfPi = Div Pi (DecInt "2")

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

-- | Takes as input a unitary gate g with four parameters, (a, b, c, d).
-- Returns a new gate with the same name, operands, and modifiers as g, and the
-- parameters (-a, -c, -b, -d). For gates such as U and CU, this function
-- computes the inverse operation to g. If G does not have three parameters,
-- then nothing is returned.
fourParamInversion :: Gate -> Maybe Gate
fourParamInversion (NamedGate name [a, b, c, d] operands mod) = Just result
    where params = [negateExpr a, negateExpr c, negateExpr b, negateExpr d]
          result = NamedGate name params operands mod
fourParamInversion _ = Nothing

-------------------------------------------------------------------------------
-- * Named Gate Inversion.

-- | Returns true if a gate is self-inverse.
gateIsSelfInverse :: Gate -> Bool
gateIsSelfInverse (NamedGate name _ _ _) = isSelfInverse name
gateIsSelfInverse (GPhaseGate _ _ _)     = False

-- | Returns true if a gate is inverted by negating its parameters.
gateIsParamInverse :: Gate -> Bool
gateIsParamInverse (NamedGate name _ _ _) = isParamInverse name
gateIsParamInverse (GPhaseGate _ _ _)     = True

-- | Takes as input a gate g. If g is not user-defined, self-inverse, nor
-- inverted by negating all parameters, and the inverse of g is known a-priori,
-- then just the inverse of g is returned (with all modifiers unchanged).
-- Otherwise, nothing is returned.
invertGateImpl :: Gate -> Maybe [Gate]
invertGateImpl (NamedGate name [] operands mod)
    | name == GateS         = Just [NamedGate GateSdg [] operands mod]
    | name == GateSdg       = Just [NamedGate GateS [] operands mod]
    | name == GateSX        = Just [NamedGate GateSX [] operands mod,
                                    NamedGate GateX [] operands mod]
    | name == GateT         = Just [NamedGate GateTdg [] operands mod]
    | name == GateTdg       = Just [NamedGate GateT [] operands mod]
    | name == GateQuipOmega = Just [GPhaseGate sevenFourthsPi operands mod]
    | name == GateQuipIX    = Just [GPhaseGate Pi operands mod,
                                    NamedGate GateQuipIX [] operands mod]
    | name == GateQuipE     = Just [GPhaseGate fiveFourthsPi operands mod,
                                    NamedGate GateH [] operands mod,
                                    NamedGate GateS [] operands mod]
    | otherwise             = Nothing
invertGateImpl (NamedGate name [a, b] operands mod)
    | name == GateU2 = maybeWrap $ threeParamInversion gate
    | otherwise      = Nothing
    where gate = NamedGate GateU3 [halfPi, a, b] operands mod
invertGateImpl (NamedGate name [a, b, c] operands mod)
    | name == GateU  = maybeWrap $ threeParamInversion gate
    | name == GateU3 = maybeWrap $ threeParamInversion gate
    | otherwise      = Nothing
    where gate = NamedGate name [a, b, c] operands mod
invertGateImpl (NamedGate name [a, b, c, d] operands mod)
    | name == GateCU = maybeWrap $ fourParamInversion gate
    | otherwise      = Nothing
    where gate = NamedGate name [a, b, c, d] operands mod
invertGateImpl _ = Nothing

-- | Takes as input a gate g. If g is not modified by the inverse flag, then g
-- is returned unchanged. If g is marked by the inverse flag, g is not
-- user-defined, and the inverse of g is known a-priori, then just the inverse
-- of g is returned (with the inverse flag lowered, and all other modifiers
-- unchanged). Otherwise, nothing is returned.
invertGate :: Gate -> Maybe [Gate]
invertGate g
    | not (isInverted g)   = Just [g]
    | gateIsSelfInverse g  = Just [invG]
    | gateIsParamInverse g = Just [negateParams invG]
    | otherwise            = invertGateImpl invG
    where invG = invert g
