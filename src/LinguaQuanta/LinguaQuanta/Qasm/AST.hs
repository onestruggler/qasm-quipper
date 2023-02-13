-- | Abstract representation of an OpenQASM 3 program.

module LinguaQuanta.Qasm.AST
  ( AstStmt(..)
  , assign
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.Gate (Gate)
import LinguaQuanta.Qasm.Language (Type)
import LinguaQuanta.Qasm.Operand
  ( Operand(..)
  , RValue
  , VoidCall
  )

-------------------------------------------------------------------------------
-- * AST Types.

-- | Abstract representations of OpenQASM statements used for program analysis
-- and source-to-source translations.
data AstStmt = AstGateStmt Int Gate
             | AstQubitDecl (Maybe Int) String
             | AstBitDecl (Maybe Int) String
             | AstAssign String (Maybe Int) RValue
             | AstCall VoidCall
             deriving (Show, Eq)

-------------------------------------------------------------------------------
-- * AST Helper Methods.

-- | Takes as input an operand (op) and an RValue (rval). Returns an AST
-- statement equivalent to: op = rval;
assign :: Operand -> RValue -> AstStmt
assign (QRef name)       rval = AstAssign name Nothing rval
assign (Cell name index) rval = AstAssign name (Just index) rval
