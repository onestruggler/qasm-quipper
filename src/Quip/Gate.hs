-- | Abstract representation of Quipper gates.

module Quip.Gate
  ( Control(..)
  , Gate(..)
  , Wire
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import Quip.GateName
  ( GateName(..)
  , RotName(..)
  )

-------------------------------------------------------------------------------
-- * Utility Types.

-- | Identifies a uniuqe wire.
type Wire = Int

-- | Associates a negative or positive polarity with a wire, so that it can act
-- as a control.
data Control = Pos Wire
             | Neg Wire
             deriving (Show, Eq)

-- | If true, then the gate is inverted.
type InverseFlag = Bool

-- | The angle parameterizing a rotational gate.
type Angle = Double

-------------------------------------------------------------------------------
-- * Quipper Gates.

-- | Abstract representations of Quipper gates. For example, nocontrol flags
-- are dropped as they are currently unsured during translation.
data Gate = NamedGate GateName InverseFlag [Wire] [Control]
          | RotGate RotName InverseFlag Angle [Wire] [Control]
          | PhaseGate Angle [Control]
          deriving (Show,Eq)
