-- | Transforms for preprocessing Quipper programs. Also provides encapsulation
-- around the Quipper transformer interface.

{-# LANGUAGE Rank2Types #-}

module Quip.Transformers
  ( applyTransformer
  , elimCtrlsTransformer
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import Data.Algebra.Boolean (xor)
import Quip.Parser (QuipCirc(..))
import Quipper
  ( Bit
  , Circ(..)
  , Ctrls(..)
  , Endpoint
  , Qubit
  , Signed(..)
  , T_Gate(..)
  , Transformer(..)
  , controlled
  , gate_H_at
  , gate_X_at
  , gate_Y_at
  , gate_Z_at
  , global_phase_anchored
  , identity_transformer
  , qmultinot_at
  , qnot_at
  , swap_at
  , transform_generic
  , without_controls_if
  )
import Quipper.Internal.Monad (named_rotation_qulist)
import Quipper.Internal.Transformer (B_Endpoint)
import Quipper.Libraries.GateDecompositions
  ( toffoli_plain_at
  , with_combined_controls
  )

-------------------------------------------------------------------------------
-- * Transformer Applications.

-- | Consumes a raw Quipper circuit, along with a Quipper circuit transformer.
-- Returns a new Quipper circuit, with the transformer applied.
applyTransformer :: Transformer Circ Qubit Bit -> QuipCirc -> QuipCirc
applyTransformer transformer (QuipCirc fn sp) = QuipCirc nfn sp
    where nfn = transform_generic transformer fn

-------------------------------------------------------------------------------
-- * Helper Methods.

-- | Represents a circuit with free controls.
type CtrlOp a = [Signed Qubit] -> Circ a

-- | Applies with_combiend_controls using the Toffoli gate.
with_combined_controls_tof :: Int -> [Signed Endpoint] -> CtrlOp a -> Circ a
with_combined_controls_tof = with_combined_controls toffoli_plain_at

-------------------------------------------------------------------------------
-- * Transformer to Eliminate Controls.

-- | Represents a list of classical and/or quantum controls.
type CtrlList = Ctrls Qubit Bit

-- | Represents a circuit returned by a transformer.
type ElimCtrlsRv = Circ ([Qubit], [Qubit], CtrlList)

-- | Implements elimCtrls for QGates.
elimCtrlsQGate :: Bool -> Int -> [Qubit] -> CtrlList -> Circ a -> ElimCtrlsRv
elimCtrlsQGate ncf n ins ctrls op =
    without_controls_if ncf $
        with_combined_controls_tof n ctrls $ \ctrls' -> do
            op `controlled` ctrls'
            return (ins, [], ctrls)

-- | The signature of a rotational circuit.
type ElimCtrlRotSig = [Qubit] -> [Qubit] -> CtrlList -> ElimCtrlsRv

-- | Implements elimCtrls for QRot gates.
elimCtrlsRotGate :: String -> Bool -> Double -> Bool -> ElimCtrlRotSig
elimCtrlsRotGate name inv ts ncf ins gens [] =
    without_controls_if ncf $ do
        named_rotation_qulist name inv ts ins gens
        return (ins, gens, [])
elimCtrlsRotGate name inv ts ncf ins gens ctrls =
    without_controls_if ncf $
        with_combined_controls_tof 1 ctrls $ \[c] -> do
            applyRot False
            qmultinot_at ins `controlled` c
            applyRot True
            qmultinot_at ins `controlled` c
            return (ins, gens, ctrls)
    where halfTs = ts / 2
          applyRot i = named_rotation_qulist name (xor inv i) halfTs ins gens

-- | An anchor used by a GPhase gate.
type Anchor = B_Endpoint Qubit Bit

-- | Implements elimCtrls for GPhase gates.
elimCtrlsGPhase :: Double -> Bool -> [Anchor] -> CtrlList -> Circ CtrlList
elimCtrlsGPhase ts ncf ins ctrls =
    without_controls_if ncf $
        with_combined_controls_tof 1 ctrls $ \ctrls' -> do
            global_phase_anchored ts ins `controlled` ctrls'
            return ctrls

-- | Quipper transformer to eliminate all controls, except for those supported
-- by OpenQASM 3. Supported controlled gates include:
-- 1. Controlled QGates: C(X), CC(X), C(Y), C(Z), C(swap), C(H), C(U)
-- 2. Controlled QRots: C(rX), C(rY), C(rZ)
-- Note that the OpenQASM 2 gate P is a singly controlled phase gate.
elimCtrlsTransformer :: Transformer Circ Qubit Bit
elimCtrlsTransformer (T_QGate "multinot" _ 0 _ ncf f) = f $
    \ins [] ctrls -> elimCtrlsQGate ncf 2 ins ctrls $ qmultinot_at ins
elimCtrlsTransformer (T_QGate "not" 1 0 _ ncf f) = f $
    \ins [] ctrls -> let [q] = ins
                     in elimCtrlsQGate ncf 2 ins ctrls $ qnot_at q
elimCtrlsTransformer (T_QGate "X" 1 0 _ ncf f) = f $
    \ins [] ctrls -> let [q] = ins
                     in elimCtrlsQGate ncf 2 ins ctrls $ gate_X_at q
elimCtrlsTransformer (T_QGate "Y" 1 0 _ ncf f) = f $
    \ins [] ctrls -> let [q] = ins
                     in elimCtrlsQGate ncf 1 ins ctrls $ gate_Y_at q
elimCtrlsTransformer (T_QGate "Z" 1 0 _ ncf f) = f $
    \ins [] ctrls -> let [q] = ins
                     in elimCtrlsQGate ncf 1 ins ctrls $ gate_Z_at q
elimCtrlsTransformer (T_QGate "swap" 2 0 _ ncf f) = f $
    \ins [] ctrls -> let [q0, q1] = ins
                     in elimCtrlsQGate ncf 1 ins ctrls $ swap_at q0 q1
elimCtrlsTransformer (T_QGate "H" 1 0 _ ncf f) = f $
    \ins [] ctrls -> let [q] = ins
                     in elimCtrlsQGate ncf 1 ins ctrls $ gate_H_at q
elimCtrlsTransformer (T_QGate n _ _ _ _ _) = error $ "Missing T_QGate:" ++ n
elimCtrlsTransformer (T_QRot name _ _ inv ts ncf f) = f $
    \ins gen ctrls -> elimCtrlsRotGate name inv ts ncf ins gen ctrls
elimCtrlsTransformer (T_GPhase ts ncf f) = f $
    \ins ctrls -> elimCtrlsGPhase ts ncf ins ctrls
elimCtrlsTransformer g = identity_transformer g
