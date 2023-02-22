-- | Functions to translate assignment statements from OpenQASM to Quipper.

module LinguaQuanta.QasmToQuip.Assign
  ( translateCDiscard
  , translateCInit0
  , translateCInit1
  , translateCTerm0
  , translateCTerm1
  , translateMeasure
  , translateQMeas
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.Operand (Operand(..))
import LinguaQuanta.QasmToQuip.Operand
  ( getWire
  , toOperand
  )
import LinguaQuanta.QasmToQuip.Ancilla
  ( translateAncilla
  , updateWireMap
  )
import LinguaQuanta.QasmToQuip.Wire
  ( WireAllocMap
  , initCell
  , initScalar
  , mvCellToCell
  , mvCellToScalar
  , mvScalarToCell
  , mvScalarToScalar
  , termCell
  , termScalar
  )
import LinguaQuanta.Quip.GateName (GateName(..))
import LinguaQuanta.Quip.Gate
  ( Control(..)
  , Gate(..)
  )

-------------------------------------------------------------------------------
-- * Ancilla Translation.

-- | Represents the gates produced by a translation, and the resulting changes
-- to the WireAllocMap.
type TranslRes = (WireAllocMap, [Gate])

-- | Takes as input a wire allocation map and the lhs of an assignment
-- (represented by a String for the declaration name, and a Maybe Int for the
-- optional array cell index). Assumes that the rhs of the assignment is a void
-- call to CDiscard, and returns the corresponding Quipper gates, together with
-- the new allocation map (the wire is terminated).
translateCDiscard :: WireAllocMap -> String -> Maybe Int -> TranslRes
translateCDiscard wmap decl idx = (wmap', gates)
    where argop = toOperand decl idx
          gates = translateAncilla wmap argop CDiscardGate
          wmap' = updateWireMap wmap argop (termScalar, termCell)

-- | Takes as input a wire allocation map and the lhs of an assignment
-- (represented by a String for the declaration name, and a Maybe Int for the
-- optional array cell index). Assumes that the rhs of the assignment is a void
-- call to CInit0, and returns the corresponding Quipper gates, together with
-- the new allocation map (the wire is initialized).
translateCInit0 :: WireAllocMap -> String -> Maybe Int -> TranslRes
translateCInit0 wmap decl idx = (wmap', gates)
    where argop = toOperand decl idx
          gates = translateAncilla wmap argop $ CInitGate False
          wmap' = updateWireMap wmap argop (initScalar, initCell)

-- | Takes as input a wire allocation map and the lhs of an assignment
-- (represented by a String for the declaration name, and a Maybe Int for the
-- optional array cell index). Assumes that the rhs of the assignment is a void
-- call to CInit1, and returns the corresponding Quipper gates, together with
-- the new allocation map (the wire is initialized).
translateCInit1 :: WireAllocMap -> String -> Maybe Int -> TranslRes
translateCInit1 wmap decl idx = (wmap', gates)
    where argop = toOperand decl idx
          gates = translateAncilla wmap argop $ CInitGate True
          wmap' = updateWireMap wmap argop (initScalar, initCell)

-- | Takes as input a wire allocation map and the lhs of an assignment
-- (represented by a String for the declaration name, and a Maybe Int for the
-- optional array cell index). Assumes that the rhs of the assignment is a void
-- call to CTerm0, and returns the corresponding Quipper gates, together with
-- the new allocation map (the wire is terminated).
translateCTerm0 :: WireAllocMap -> String -> Maybe Int -> TranslRes
translateCTerm0 wmap decl idx = (wmap', gates)
    where argop = toOperand decl idx
          gates = translateAncilla wmap argop $ CTermGate False
          wmap' = updateWireMap wmap argop (termScalar, termCell)

-- | Takes as input a wire allocation map and the lhs of an assignment
-- (represented by a String for the declaration name, and a Maybe Int for the
-- optional array cell index). Assumes that the rhs of the assignment is a void
-- call to CTerm1, and returns the corresponding Quipper gates, together with
-- the new allocation map (the wire is terminated).
translateCTerm1 :: WireAllocMap -> String -> Maybe Int -> TranslRes
translateCTerm1 wmap decl idx = (wmap', gates)
    where argop = toOperand decl idx
          gates = translateAncilla wmap argop $ CTermGate True
          wmap' = updateWireMap wmap argop (termScalar, termCell)

-------------------------------------------------------------------------------
-- * Measurement Translation.

-- | Takes as input a wire allocation map, the lhs of an assignment statement
-- (represented by a String for the declaration name, and a Maybe Int for the)
-- optional array cell index), and an operand represeting the target of a
-- measure operation (this is assigned to the lhs). Returns the Quipper gates
-- required to measure the rhs operand, and to store the result to the lhs.
translateMeasure :: WireAllocMap -> String -> Maybe Int -> Operand -> [Gate]
translateMeasure wmap decl idx qop =
    case getWire cop wmap of
        Just cw -> case getWire qop wmap of
            Just qw -> [CDiscardGate cw,
                        QInitGate False cw,
                        NamedGate GateX False [cw] [Pos qw],
                        QMeasGate cw]
            Nothing -> error $ "Failed to handle measure at: " ++ show qop
        Nothing -> error $ "Failed to handle measure to: " ++ show cop
    where cop = toOperand decl idx

-- | Takes as input a wire allocation map, the rhs of a QMeas assignment (given
-- by an operand), and the lhs of a QMeas assignment (given by an operand).
-- Expands the operands to (String, Maybe Int) pairs and then returns the
-- result of applying mv{Scalar,Cell}To{Scalar,Cell} to the two operands and
-- the wire allocation map.
qmeasUpdate :: WireAllocMap -> Operand -> Operand -> Maybe WireAllocMap
qmeasUpdate wmap (QRef srcId) (QRef dstId) = wmap'
    where wmap' = mvScalarToScalar srcId dstId wmap
qmeasUpdate wmap (QRef srcId) (Cell dstId dstIdx) = wmap'
    where wmap' = mvScalarToCell srcId dstId dstIdx wmap
qmeasUpdate wmap (Cell srcId srcIdx) (QRef dstId) = wmap'
    where wmap' = mvCellToScalar srcId srcIdx dstId wmap
qmeasUpdate wmap (Cell srcId srcIdx) (Cell dstId dstIdx) = wmap'
    where wmap' = mvCellToCell srcId srcIdx dstId dstIdx wmap

-- | Takes as input a wire allocation map, the lhs of an assignment statement
-- (represented by a String for the declaration name, and a Maybe Int for the)
-- optional array cell index), and an operand represeting the target of a
-- QMeas (this is assigned to the lhs). Returns the original Quipper gates
-- encoded by this assignment, taking into accoutn the allocation data in the
-- wire map. The wire map is updated to propogate the internal state of the
-- quantum bit to the state of the classical bit.
translateQMeas :: WireAllocMap -> String -> Maybe Int -> Operand -> TranslRes
translateQMeas wmap decl idx qop =
    case getWire qop wmap of
        Just w -> case qmeasUpdate wmap qop $ toOperand decl idx of
            Just wmap' -> (wmap', [QMeasGate w])
            Nothing    -> error $ "Failed to measure state at: " ++ show w
        Nothing -> error $  "Failed to find wire for measurement: " ++ show qop
