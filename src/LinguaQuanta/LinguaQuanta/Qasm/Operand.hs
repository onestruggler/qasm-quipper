-- | Common information about operands, used by both expressions and gates.

module LinguaQuanta.Qasm.Operand where

-------------------------------------------------------------------------------
-- * Definitions.

-- | Abstract representation of a gate operand. In particular, all array cells
-- must be compile-time constants.
data Operand = QRef String
             | Cell String Int
             deriving (Show, Eq)

-- | Abstract representation of the right-hand side of an assignment. These
-- often depend on one or more operands.
data RValue = QuipMeasure Operand
            | QuipCInit0
            | QuipCInit1
            | QuipCTerm0
            | QuipCTerm1
            | QuipCDiscard
            | Measure Operand
            deriving (Show, Eq)

-- | Abstract represenation of a void function call that appears in a concrete
-- expression statement.
data VoidCall = QuipQInit0 Operand
              | QuipQInit1 Operand
              | QuipQTerm0 Operand
              | QuipQTerm1 Operand
              | QuipQDiscard Operand
              | VoidReset Operand
              | VoidMeasure Operand
              deriving (Show, Eq)
