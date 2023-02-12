-- | Functions to translate Quipper measurements to OpenQASM functions.

module LinguaQuanta.QuipToQasm.Measurement (translateMeasurement) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.AST
  ( AstStmt(..)
  , assign
  )
import LinguaQuanta.Qasm.Operand
  ( Operand
  , RValue(..)
  )
import LinguaQuanta.Quip.Gate (Wire)
import LinguaQuanta.Quip.Wire (WireType(..))
import LinguaQuanta.QuipToQasm.Wire
  ( WireLookup
  , collapseState
  , getAllocation
  )

-------------------------------------------------------------------------------
-- * Measurement Translation Utilities.

-- | Takes as input a wire lookup (wmap) and a wire (w). If w can collapse to a
-- previously allocated qubit, then w is collapsed in a wmap, and wmap is
-- returned. If w collapses to a newly allocated qubit, then the declaration
-- name is returned alongside the new wmap. If state collapse fails, then the
-- corresponding error is raised.
prepareClassicalBit :: WireLookup -> Wire -> (WireLookup, [AstStmt])
prepareClassicalBit wmap w =
    case collapseState w wmap of
        Right err               -> error $ "translateMeasurement: " ++ show err
        Left (wmap', Nothing)   -> (wmap', [])
        Left (wmap', Just name) -> (wmap', [AstBitDecl Nothing name])

-- | Takes as input a qubit operand (qubit), a wire lookup, and the index of
-- the qubit (w). Uses the wire lookup to return a statement equivalent to
-- Meas(w). In particular, the w-th CWire will be set to QMeas(qubit). If the
-- classical wire is undeclared, then an error is raised.
measureAndAssign :: Operand -> WireLookup -> Wire -> (WireLookup, [AstStmt])
measureAndAssign qubit wmap w =
    case getAllocation CWire w wmap' of
        Just cbit -> let astmt = assign cbit $ QuipMeasure qubit
                     in (wmap', dstmts ++ [astmt])
        Nothing   -> error "translateMeasurement: cwire unknown" 
    where (wmap', dstmts) = prepareClassicalBit wmap w

-------------------------------------------------------------------------------
-- * Measurement Translation.

-- | Takes as input a wire lookup and the index of a wire. Returns the OpenQASM
-- statements required to simulate a Quipper measurement of wire w, together
-- with an updated wire lookup. If the wire is undeclared, or the typing is
-- incorrected, then an error is raised.
translateMeasurement :: WireLookup -> Wire -> (WireLookup, [AstStmt])
translateMeasurement wmap w =
    case getAllocation QWire w wmap of
        Just qubit -> measureAndAssign qubit wmap w
        Nothing    -> error "translateMeasurement: qwire unknown" 
