-- | Abstract representation of Quipper gates.

module LinguaQuanta.Quip.Gate
  ( Control(..)
  , Gate(..)
  , Wire
  , toWires
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Quip.GateName
  ( GateName(..)
  , RotName(..)
  )

-------------------------------------------------------------------------------
-- * Utility Types.

-- | Identifies a uniuqe wire.
type Wire = Int

-- | If true, then the gate is inverted.
type InverseFlag = Bool

-- | If true, then the initialized/terminated register is 1, otherwise 0.
type SetOnFlag = Bool

-- | The angle parameterizing a rotational gate.
type Angle = Double

-------------------------------------------------------------------------------
-- * Control Wires.

-- | Associates a negative or positive polarity with a wire, so that it can act
-- as a control.
data Control = Pos Wire
             | Neg Wire
             deriving (Show, Eq)

-- | Takes as input a list of controlled wires. Returns the corresponding list
-- of uncontrolled wires.
toWires :: [Control] -> [Wire]
toWires []              = []
toWires ((Pos w):wires) = w : (toWires wires)
toWires ((Neg w):wires) = w : (toWires wires)

-------------------------------------------------------------------------------
-- * Quipper Gates.

-- | Abstract representations of Quipper gates. For example, nocontrol flags
-- are dropped as they are currently unsured during translation.
data Gate = NamedGate GateName InverseFlag [Wire] [Control]
          | RotGate RotName InverseFlag Angle [Wire] [Control]
          | PhaseGate Angle [Control]
          | QInitGate SetOnFlag Wire
          | QTermGate SetOnFlag Wire
          | QDiscardGate Wire
          | CInitGate SetOnFlag Wire
          | CTermGate SetOnFlag Wire
          | CDiscardGate Wire
          | QMeasGate Wire
          deriving (Show,Eq)
