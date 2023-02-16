-- | Functions to translate ancilla-related gates from Quipper to OpenQASM.

module LinguaQuanta.QuipToQasm.Ancilla
  ( translateCDiscard
  , translateCInit
  , translateCTerm
  , translateQDiscard
  , translateQInit
  , translateQTerm
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.AST
  ( AstStmt(..)
  , assign
  )
import LinguaQuanta.Qasm.Operand
  ( Operand(..)
  , RValue(..)
  , VoidCall(..)
  )
import LinguaQuanta.Quip.Gate (Wire)
import LinguaQuanta.Quip.Wire (WireType(..))
import LinguaQuanta.QuipToQasm.Wire
  ( WireLookup
  , getAllocation
  , initCBit
  , initQBit
  , termCBit
  , termQBit
  )

-------------------------------------------------------------------------------
-- * Ancilla Translation Utilities.

-- | Implementation details for translateQInit. Determines if a new declaration
-- is required.
translateQInitImpl :: Operand -> Bool -> Maybe String -> [AstStmt]
translateQInitImpl op isOn Nothing = [AstCall $ call op]
    where call = if isOn then QuipQInit1 else QuipQInit0
translateQInitImpl op isOn (Just name) = dstmt:cstmts
    where dstmt  = AstQubitDecl Nothing name
          cstmts = translateQInitImpl op isOn Nothing

-- | Implementation details for translateCInit. Determines if a new declaration
-- is required.
translateCInitImpl :: Operand -> Bool -> Maybe String -> [AstStmt]
translateCInitImpl op isOn Nothing = [assign op call]
    where call = if isOn then QuipCInit1 else QuipCInit0
translateCInitImpl op isOn (Just name) = dstmt:cstmts
    where dstmt  = AstBitDecl Nothing name
          cstmts = translateCInitImpl op isOn Nothing

-- | Implementation details for translateQInit and translateCInit. Determines
-- the operand.
getOperand :: Maybe String -> WireType -> Wire -> WireLookup -> Operand
getOperand Nothing ty id lookup =
    case getAllocation ty id lookup of
        Just op -> op
        Nothing -> error $ "translate{Q,C}Init: missing wire " ++ show id
getOperand (Just name) _ _ _ = QRef name

-------------------------------------------------------------------------------
-- * Ancilla Translation.

-- | Takes as input a wire lookup (lookup), the index of an unused wire in the
-- lookup (id), and a flag (isOn). If isOn is true, then returns a sequence of
-- OpenQASM statements equivalent to QInit1(id). Otherwise, a sequence of
-- OpenQASM statements equivalent to QInit0(id) are returned. The lookup is
-- used to resolve the identifier and validate the gate. The updated lookup is
-- also returns. If validation fails, then an error is raised.
translateQInit :: WireLookup -> Wire -> Bool -> (WireLookup, [AstStmt])
translateQInit lookup id isOn =
    case initQBit id lookup of 
        Left (lookup', name) -> let op    = getOperand name QWire id lookup
                                    stmts = translateQInitImpl op isOn name
                                in (lookup', stmts)
        Right err -> error $ "translateQInit: " ++ show err

-- | Takes as input a wire lookup (lookup), the index of an active qwire in the
-- lookup (id), and a flag (isOn). If isOn is true, then returns a sequence of
-- OpenQASM statements equivalent to QTerm1(id). Otherwise, a sequence of of
-- OpenQASM statements equivalent to QInit0(id) are returned. The lookup is
-- used to resolve the identifier and validate the gate. The updated lookup is
-- also returns. If validation fails, then an error is raised.
translateQTerm :: WireLookup -> Wire -> Bool -> (WireLookup, [AstStmt])
translateQTerm lookup id isOn =
    case getAllocation QWire id lookup of
        Just op -> case termQBit id lookup of
            Left lookup' -> let call = if isOn then QuipQTerm1 else QuipQTerm0
                                stmt = AstCall $ call op
                            in (lookup', [stmt])
            Right err -> error $ "translateQTerm: " ++ show err
        Nothing -> error $ "translateQTerm: missing wire " ++ show id

-- | Takes as input a wire lookup (lookup) and the index of an active qwire in
-- the lookup (id). Returns a sequence of OpenQASM statements equivalent to
-- QDiscard(id). The lookup is used to resolve the identifier and validate the
-- gate. The updated lookup is also returns. If validation fails, then an error
-- is raised.
translateQDiscard :: WireLookup -> Wire -> (WireLookup, [AstStmt])
translateQDiscard lookup id =
    case getAllocation QWire id lookup of
        Just op -> case termQBit id lookup of
            Left lookup' -> let stmt = AstCall $ QuipQDiscard op
                            in (lookup', [stmt])
            Right err -> error $ "translateQDiscard: " ++ show err
        Nothing -> error $ "translateQDiscard: missing wire " ++ show id

-- | Same as translateQInit, except for CInit0 and CInit1.
translateCInit :: WireLookup -> Wire -> Bool -> (WireLookup, [AstStmt])
translateCInit lookup id isOn =
    case initCBit id lookup of 
        Left (lookup', name) -> let op    = getOperand name CWire id lookup
                                    stmts = translateCInitImpl op isOn name
                                in (lookup', stmts)
        Right err -> error $ "translateCInit: " ++ show err

-- | Same as translateQTerm, except for CTerm0 and CTerm1.
translateCTerm :: WireLookup -> Wire -> Bool -> (WireLookup, [AstStmt])
translateCTerm lookup id isOn =
    case getAllocation CWire id lookup of
        Just op -> case termCBit id lookup of
            Left lookup' -> let call = if isOn then QuipCTerm1 else QuipCTerm0
                                stmt = assign op call
                            in (lookup', [stmt])
            Right err -> error $ "translateCTerm: " ++ show err
        Nothing -> error $ "translateCTerm: missing wire " ++ show id

-- | Same as translateQDiscard, except for CDiscard.
translateCDiscard :: WireLookup -> Wire -> (WireLookup, [AstStmt])
translateCDiscard lookup id =
    case getAllocation CWire id lookup of
        Just op -> case termCBit id lookup of
            Left lookup' -> let stmt = assign op QuipCDiscard
                            in (lookup', [stmt])
            Right err -> error $ "translateCDiscard: " ++ show err
        Nothing -> error $ "translateCDiscard: missing wire " ++ show id
