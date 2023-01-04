-- | The top-level functions to translate OpenQASM to Quipper

module LinguaQuanta.QasmToQuip.Translator (translate) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.AST (AstStmt(..))
import LinguaQuanta.QasmToQuip.Wire
  ( WireAllocMap
  , allocate
  , initialAllocations
  , toQuipperInputs
  , toQuipperOutputs
  , toSize
  )
import LinguaQuanta.Quip.Gate (Gate)
import LinguaQuanta.Quip.Wire (WireType(..))
import LinguaQuanta.Quip.Quipper (GateCirc(..))

-------------------------------------------------------------------------------
-- * Statement Translation.

-- | Takes as input an allocation map and an AST statement. Returns the
-- allocation map obtained by applying the statement, and a list of gates
-- corresponding to the statement.
translateStmt :: WireAllocMap -> AstStmt -> (WireAllocMap, [Gate])
translateStmt map (AstQubitDecl size name) =
    case allocate QWire name size map of
        Nothing   -> error $ "Duplication allocation: " ++ name
        Just map' -> (map', [])
translateStmt map (AstGateStmt _ _) = error "AstGateStmt transl unimplemented."

-- | Takes as input an allocation map and a list of AST statements. Returns the
-- allocation map obtained by applying all statements in order, and a list of
-- gates corresponding to the list of statements. 
translateStmts :: WireAllocMap -> [AstStmt] -> (WireAllocMap, [Gate])
translateStmts map []           = (map, [])
translateStmts map (stmt:stmts) = (map'', gates ++ rest)
    where (map', gates) = translateStmt map stmt
          (map'', rest) = translateStmts map' stmts

-------------------------------------------------------------------------------
-- * Top-Level Translation Functions

-- | Takes as input an abstract OpenQASM circuit. Returns an equivalent Quipper
-- gate circuit.
translate :: [AstStmt] -> GateCirc
translate circ = GateCirc { inputs  = toQuipperInputs map
                          , gates   = gates
                          , outputs = toQuipperOutputs map
                          , size    = toSize map
                          }
    where (map, gates) = translateStmts initialAllocations circ
