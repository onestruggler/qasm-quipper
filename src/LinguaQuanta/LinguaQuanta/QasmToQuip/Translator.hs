-- | The top-level functions to translate OpenQASM to Quipper

module LinguaQuanta.QasmToQuip.Translator (translate) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.AST (AstStmt(..))
import LinguaQuanta.Qasm.Gate as Qasm
import LinguaQuanta.QasmToQuip.Gate
  ( d1RotTransl
  , d2RotTransl
  , d3RotTransl
  , namedGateTransl
  , translGPhase
  )
import LinguaQuanta.QasmToQuip.Wire
  ( CellUpdate
  , ScalarUpdate
  , WireAllocMap
  , allocate
  , initialAllocations
  , toQuipperInputs
  , toQuipperOutputs
  , toSize
  , useCell
  , useScalar
  )
import LinguaQuanta.Quip.Gate as Quip
import LinguaQuanta.Quip.Wire (WireType(..))
import LinguaQuanta.Quip.Quipper (GateCirc(..))

-------------------------------------------------------------------------------
-- * Gate Translation.

-- |
type UpdatePair = (ScalarUpdate, CellUpdate)

-- | Takes as input an allocation map (wmap), a gate repetition count (n), and
-- a Quipper gate (g). Returns a tuple (ops, gates) where ops contains the
-- operands for g, and gates contains n copies of the Quipper gate equivalent
-- to g with respect to wmap.
translateGate :: WireAllocMap -> Int -> Qasm.Gate -> ([Operand], [Quip.Gate])
translateGate wmap 0 (Qasm.NamedGate name [] ops mod) = (ops, stmts)
    where stmts = namedGateTransl wmap name ops mod
translateGate wmap 0 (Qasm.NamedGate name [p] ops mod) = (ops, stmts)
    where stmts = d1RotTransl wmap name p ops mod
translateGate wmap 0 (Qasm.NamedGate name [p1, p2] ops mod) = (ops, stmts)
    where stmts = d2RotTransl wmap name (p1, p2) ops mod
translateGate wmap 0 (Qasm.NamedGate name [p1, p2, p3] ops mod) = (ops, stmts)
    where stmts = d3RotTransl wmap name (p1, p2, p3) ops mod
translateGate wmap 0 (Qasm.NamedGate name params ops mod) = error msg
    where msg = "Rotations must not exceed three dimensions."
translateGate wmap 0 (Qasm.GPhaseGate param ops mod) = (ops, stmts)
    where stmts = translGPhase wmap param ops mod
translateGate wmap n gate = (ops, concat $ replicate n $ gates)
    where (ops, gates) = translateGate wmap 0 gate

-- | Takes as input a pair of update functions and an operand. Determines the
-- type of operand, and applies the correct update function.
getUpdateFn :: UpdatePair -> Operand -> (WireAllocMap -> Maybe WireAllocMap)
getUpdateFn (fn, _) (QRef id)     = fn id
getUpdateFn (_, fn) (Cell id idx) = fn id idx

-- | Takes as input an allocation map (wmap), a pair of update functions (fns),
-- and a list of operands (ops). For each operand, applies the corresponding
-- update in fns to wmap. Returns the resulting wmap.
--
-- Note: If an operand from ops is not in wmap, then an error is raised.
updateOperands :: WireAllocMap -> UpdatePair -> [Operand] -> WireAllocMap
updateOperands wmap _   []       = wmap
updateOperands wmap fns (op:ops) =
    case update wmap of
        Nothing    -> error errmsg
        Just wmap' -> updateOperands wmap' fns ops
    where update = getUpdateFn fns op
          errmsg = "Failed to fail operand after applying gate to operand."

-------------------------------------------------------------------------------
-- * Statement Translation.

-- | Takes as input an allocation map and an AST statement. Returns the
-- allocation map obtained by applying the statement, and a list of gates
-- corresponding to the statement.
translateStmt :: WireAllocMap -> AstStmt -> (WireAllocMap, [Quip.Gate])
translateStmt wmap (AstQubitDecl size name) =
    case allocate QWire name size wmap of
        Nothing   -> error $ "Duplication allocation: " ++ name
        Just wmap' -> (wmap', [])
translateStmt wmap (AstGateStmt n gate) =  (wmap', gates)
    where (ops, gates) = translateGate wmap n gate
          wmap'        = updateOperands wmap (useScalar, useCell) ops

-- | Takes as input an allocation map and a list of AST statements. Returns the
-- allocation map obtained by applying all statements in order, and a list of
-- gates corresponding to the list of statements. 
translateStmts :: WireAllocMap -> [AstStmt] -> (WireAllocMap, [Quip.Gate])
translateStmts wmap []           = (wmap, [])
translateStmts wmap (stmt:stmts) = (wmap'', gates ++ rest)
    where (wmap', gates) = translateStmt wmap stmt
          (wmap'', rest) = translateStmts wmap' stmts

-------------------------------------------------------------------------------
-- * Top-Level Translation Functions

-- | Takes as input an abstract OpenQASM circuit. Returns an equivalent Quipper
-- gate circuit.
translate :: [AstStmt] -> GateCirc
translate circ = GateCirc { inputs  = toQuipperInputs wmap
                          , gates   = gates
                          , outputs = toQuipperOutputs wmap
                          , size    = toSize wmap
                          }
    where (wmap, gates) = translateStmts initialAllocations circ
