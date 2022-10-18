-- | Abstract representation of OpenQasm 3 gates and their modifiers.

module Qasm.Gate
  ( Sign(..)
  , GateMod(..)
  , nullGateMod
  , negateMod
  , addCtrlsToMod
  , addNegCtrlsToMod
  ) where

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
