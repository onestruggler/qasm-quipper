-- | Functions to compute inverse gates.

module Qasm.Inversion
  ( negateParams
  , oneParamInversion
  , threeParamInversion
  , invertGate
  ) where

import Qasm.Expression (zero, negateExpr, avgExpr)
import Qasm.Gate (GateMod, Gate(..), invert, isInverted)
import Qasm.GateName (GateName(..), toOperandCount, isSelfInverse, isParamInverse)
import Qasm.Language (Expr(..), GateOperand)
import Utils (maybeWrap, maybeAppend)

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

-- | Takes as input a unitary gate g with one parameter (a), and a flag which
-- indicates if the gate is controlled (isControlled). If the isControlled flag
-- is false, then returns the just gate U(0, a, 0) with the same modifiers as
-- g. If the isControlled flag is true, then returns just the  gate CU(0, a, 0)
-- with the same modifiers as g. Otherwise, nothing is returned.
--
-- Note: If isControlled is set, then g must take two operands to match CU.
-- Otherwise, g must take a single operand to match U. If this requirement is
-- violated, then nothing is returned.
oneParamInversion :: Gate -> Bool -> Maybe Gate
oneParamInversion (NamedGate name [a] operands mod) isControlled
    | actOpCt == expOpCt = Just (NamedGate newName params operands mod)
    | otherwise          = Nothing
    where actOpCt = toOperandCount name
          expOpCt = if isControlled then Just 2 else Just 1
          newName = if isControlled then GateCU else GateU
          params  = [zero, negateExpr a, zero]
oneParamInversion _ _ = Nothing

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

-- | Takes as input the three parameters of a u3 gate (a, b, c), its operands,
-- and its modifiers (mod). If the u3 gate is valid, then returns the inverse
-- of u3(a, b, c) with the modifiers mod (i.e., a phase gate of angle (b+c)/2,
-- followed by the inverse of U(a,b,c)). Otherwise, nothing is returned.
invertU3 :: Expr -> Expr -> Expr -> [GateOperand] -> GateMod -> Maybe [Gate]
invertU3 a b c operands mod = maybeAppend phase mcirc
    where phase = GPhaseGate (avgExpr b c) operands mod
          ucomp = NamedGate GateU [a, b, c] operands mod
          mcirc = maybeWrap $ threeParamInversion ucomp

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
invertGateImpl (NamedGate name [a] operands mod)
    | name == GateU1     = maybeWrap $ oneParamInversion gate False
    | name == GatePhase  = maybeWrap $ oneParamInversion gate False
    | name == GateCPhase = maybeWrap $ oneParamInversion gate True
    | otherwise          = Nothing
    where gate = NamedGate name [a] operands mod
invertGateImpl (NamedGate name [a, b] operands mod)
    | name == GateU2 = invertU3 halfPi a b operands mod
    | otherwise      = Nothing
    where comp = NamedGate GateU [halfPi, a, b] operands mod
invertGateImpl (NamedGate name [a, b, c] operands mod)
    | name == GateU  = maybeWrap $ threeParamInversion gate
    | name == GateCU = maybeWrap $ threeParamInversion gate
    | name == GateU3 = invertU3 a b c operands mod
    | otherwise      = Nothing
    where gate = NamedGate name [a, b, c] operands mod
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
