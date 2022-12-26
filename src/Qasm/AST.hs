-- | Abstract representation of an OpenQASM 3 program.

module Qasm.AST where

-------------------------------------------------------------------------------
-- * Import Section.

import Qasm.Gate (Gate(..))
import Qasm.Language (Type(..))

-------------------------------------------------------------------------------
-- * AST Types.

data AstStmt = AstGateStmt Int Gate
             | AstQubitDecl (Maybe Int) String
             deriving (Show, Eq)
