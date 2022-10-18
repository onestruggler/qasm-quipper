-- | Abstract representation of OpenQasm 3 gates and their modifiers.

module Qasm.Gate
  ( Sign(..)
  , GateMod(..)
  , nullGateMod
  , negateMod
  , addCtrlsToMod
  , addNegCtrlsToMod
  , Gate(..)
  , invert
  , addCtrls
  , addNegCtrls
  ) where

import Qasm.GateName (GateName)
import Qasm.Language (Expr(..), GateOperand(..), GateExpr(..))

-------------------------------------------------------------------------------
-- * Modifiers and Update Functions.

-- | If true, then the gate is inverted.
type InverseFlag = Bool

-- | Represents the polarity of a control.
data Sign = Pos | Neg deriving (Show, Eq)

-- | Summarizes the modifiers applied to a gate.
data GateMod = GateMod InverseFlag [Sign] deriving (Show, Eq)

-- | Default initializes a gate modifier (no controls and not inverted).
nullGateMod :: GateMod
nullGateMod = GateMod False []

-- | Negates the inversion flag of a gate modifier. The positive and negative
-- controls are left unchanged.
negateMod :: GateMod -> GateMod
negateMod (GateMod inv ctrls) = GateMod (not inv) ctrls

-- | Consumes a count (n), a control polarity (sign), and a gate modifier.
-- Returns a new gate modifier, with n copies of sign prepended to the control
-- list, and the inverse flag left unchanged.
addSignsToMod :: Sign -> Int -> GateMod -> GateMod
addSignsToMod sign n (GateMod inv ctrls) = GateMod inv updated
    where updated = take n (repeat sign) ++ ctrls

-- Equivalent to (addSignsToMod Pos).
addCtrlsToMod :: Int -> GateMod -> GateMod
addCtrlsToMod = addSignsToMod Pos

-- Equivalent to (addSignsToMod Neg).
addNegCtrlsToMod :: Int -> GateMod -> GateMod
addNegCtrlsToMod = addSignsToMod Neg

-------------------------------------------------------------------------------
-- * Gates and Decorator Functions.

-- | Abstract representation of a gate: name, parameters, operands, and mods.
data Gate = NamedGate GateName [Expr] [GateOperand] GateMod
          | GPhaseGate Expr [GateOperand] GateMod
          deriving (Show, Eq)

-- | Consumes a modifier update function (f) and a gate. Returns the gate
-- obtained by applying f to the underlying gate modifier.
applyToMod :: (GateMod -> GateMod) -> Gate -> Gate
applyToMod f (NamedGate name params operands mod)
    = NamedGate name params operands (f mod)
applyToMod f (GPhaseGate param operands mod)
    = GPhaseGate param operands (f mod)

-- Equivalent to applyToMod negateMod.
invert :: Gate -> Gate
invert = applyToMod negateMod

-- | Equivalent to applyToMod (addCtrlsToMod n).
addCtrls :: Int -> Gate -> Gate
addCtrls n = applyToMod (addCtrlsToMod n)

-- | Equivalent to applyToMod (addNegCtrlsToMod n).
addNegCtrls :: Int -> Gate -> Gate
addNegCtrls n gate = applyToMod (addNegCtrlsToMod n) gate
