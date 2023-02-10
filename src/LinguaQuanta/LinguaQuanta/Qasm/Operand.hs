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
data RValue = QuipMeasure Operand deriving (Show, Eq)
