-- | Abstract representation of an OpenQASM 3 program.

module LinguaQuanta.Qasm.AST where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.Gate (Gate(..))
import LinguaQuanta.Qasm.Language (Type(..))

-------------------------------------------------------------------------------
-- * AST Types.

data AstStmt = AstGateStmt Int Gate
             | AstQubitDecl (Maybe Int) String
             | AstBitDecl (Maybe Int) String
             deriving (Show, Eq)
