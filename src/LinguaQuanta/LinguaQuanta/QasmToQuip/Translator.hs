-- | The top-level functions to translate OpenQASM to Quipper

module LinguaQuanta.QasmToQuip.Translator (translate) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.AST (AstStmt(..))
import LinguaQuanta.Qasm.Gate as Qasm
import LinguaQuanta.Qasm.Operand
  ( RValue(..)
  , VoidCall(..)
  )
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
-- * Utilities.

-- | Represents the gates produced by a translation, and the resulting changes
-- to the WireAllocMap.
type TranslRes = (WireAllocMap, [Quip.Gate])

-- | Encodes update functions for both QRef and Cell.
type UpdatePair = (ScalarUpdate, CellUpdate)

-- | Takes as input a pair of update functions and an operand. Determines the
-- type of operand, and applies the correct update function.
getUpdateFn :: UpdatePair -> Operand -> (WireAllocMap -> Maybe WireAllocMap)
getUpdateFn (fn, _) (QRef id)     = fn id
getUpdateFn (_, fn) (Cell id idx) = fn id idx

-------------------------------------------------------------------------------
-- * Call Translation.

-- | Takes as input the current allocation map and a void call. Returns the
-- set of gates corresponding to the call, together with an updated wire
-- allocation map (according to the semantics of the call). If the call is not
-- supported in translation, then an error is raised.
translateCall :: WireAllocMap -> VoidCall -> TranslRes
translateCall wmap (QuipQInit0 op) = error msg
    where msg = "QInit0 translation not implemented."
translateCall wmap (QuipQInit1 op) = error msg
    where msg = "QInit1 translation not implemented."
translateCall wmap (QuipQTerm0 op) = error msg
    where msg = "QTerm0 translation not implemented."
translateCall wmap (QuipQTerm1 op) = error msg
    where msg = "QTerm1 translation not implemented."
translateCall wmap (QuipQDiscard op) = error msg
    where msg = "QDiscard translation not implemented."

-------------------------------------------------------------------------------
-- * Assignment Translation.

-- | Takes as input the current allocation map, the name of a declaration that
-- should be overwritten (optionally with an index), and the r-value with which
-- the declaration should be overwritten.
translateAssign :: WireAllocMap -> String -> Maybe Int -> RValue -> TranslRes
translateAssign wmap id idx QuipCInit0 = error msg
    where msg = "QInit0 translation not implemented."
translateAssign wmap id idx QuipCInit1 = error msg
    where msg = "QInit1 translation not implemented."
translateAssign wmap id idx QuipCTerm0 = error msg
    where msg = "QTerm0 translation not implemented."
translateAssign wmap id idx QuipCTerm1 = error msg
    where msg = "QTerm1 translation not implemented."
translateAssign wmap id idx QuipCDiscard = error msg
    where msg = "QDiscard translation not implemented."
translateAssign wmap id idx (QuipMeasure op) = error msg
    where msg = "QMeas translation not implemented."

-------------------------------------------------------------------------------
-- * Gate Translation.

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
translateStmt :: WireAllocMap -> AstStmt -> TranslRes
translateStmt wmap (AstQubitDecl size name) =
    case allocate QWire name size wmap of
        Nothing    -> error $ "Duplication qubit allocation: " ++ name
        Just wmap' -> (wmap', [])
translateStmt wmap (AstBitDecl size name) =
    case allocate CWire name size wmap of
        Nothing    -> error $ "Duplicate bit allocation: " ++ name
        Just wmap' -> (wmap', [])
translateStmt wmap (AstGateStmt n gate) = (wmap', gates)
    where (ops, gates) = translateGate wmap n gate
          wmap'        = updateOperands wmap (useScalar, useCell) ops
translateStmt wmap (AstAssign decl index rval) = res
    where res = translateAssign wmap decl index rval
translateStmt wmap (AstCall call) = res
    where res = translateCall wmap call

-- | Takes as input an allocation map and a list of AST statements. Returns the
-- allocation map obtained by applying all statements in order, and a list of
-- gates corresponding to the list of statements. 
translateStmts :: WireAllocMap -> [AstStmt] -> TranslRes
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
