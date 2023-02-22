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
import LinguaQuanta.QasmToQuip.Call
  ( translateQDiscard
  , translateQInit0
  , translateQInit1
  , translateQTerm0
  , translateQTerm1
  , translateReset
  )
import LinguaQuanta.QasmToQuip.Gate
  ( d1RotTransl
  , d2RotTransl
  , d3RotTransl
  , namedGateTransl
  , translGPhase
  )
import LinguaQuanta.QasmToQuip.Assign
  ( translateCDiscard
  , translateCInit0
  , translateCInit1
  , translateCTerm0
  , translateCTerm1
  , translateMeasure
  )
import LinguaQuanta.QasmToQuip.Operand
  ( UpdatePair
  , getUpdateFn
  )
import LinguaQuanta.QasmToQuip.Wire
  ( WireAllocMap
  , allocate
  , hasLoans
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

-------------------------------------------------------------------------------
-- * Call Translation.

-- | Takes as input the current allocation map and a void call. Returns the
-- set of gates corresponding to the call, together with an updated wire
-- allocation map (according to the semantics of the call). If the call is not
-- supported in translation, then an error is raised.
translateCall :: WireAllocMap -> VoidCall -> TranslRes
translateCall wmap (QuipQInit0 op)   = translateQInit0 wmap op
translateCall wmap (QuipQInit1 op)   = translateQInit1 wmap op
translateCall wmap (QuipQTerm0 op)   = translateQTerm0 wmap op
translateCall wmap (QuipQTerm1 op)   = translateQTerm1 wmap op
translateCall wmap (QuipQDiscard op) = translateQDiscard wmap op
translateCall wmap (VoidReset op)    = translateReset wmap op
translateCall wmap (VoidMeasure op)  = error msg
    where msg = "VoidMeasure translation not implemented."

-------------------------------------------------------------------------------
-- * Assignment Translation.

-- | Takes as input the current allocation map, the name of a declaration that
-- should be overwritten (optionally with an index), and the r-value with which
-- the declaration should be overwritten.
translateAssign :: WireAllocMap -> String -> Maybe Int -> RValue -> TranslRes
translateAssign wmap id idx QuipCInit0       = translateCInit0 wmap id idx
translateAssign wmap id idx QuipCInit1       = translateCInit1 wmap id idx
translateAssign wmap id idx QuipCTerm0       = translateCTerm0 wmap id idx
translateAssign wmap id idx QuipCTerm1       = translateCTerm1 wmap id idx
translateAssign wmap id idx QuipCDiscard     = translateCDiscard wmap id idx
translateAssign wmap id idx (QuipMeasure op) = error msg
    where msg = "QMeas translation not implemented."
translateAssign wmap id idx (Measure op) = (wmap, gates)
    where gates = translateMeasure wmap id idx op

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
translate circ
    | hasLoans wmap = error "Loaned wire not returend to wire allocation map."
    | otherwise     = GateCirc { inputs  = toQuipperInputs wmap
                               , gates   = gates
                               , outputs = toQuipperOutputs wmap
                               , size    = toSize wmap
                               }
    where (wmap, gates) = translateStmts initialAllocations circ
