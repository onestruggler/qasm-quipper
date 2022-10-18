-- | Abstract representation of an OpenQASM 3 program.

module Qasm.AST where

import Qasm.Gate (Gate(..))
import Qasm.Language (Type(..))

data AstStmt = AstGateStmt Gate
             | AstDeclStmt Type String
             deriving (Show, Eq)
