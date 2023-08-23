-- | Transforms for preprocessing Quipper programs. Also provides encapsulation
-- around the Quipper transformer interface.

{-# LANGUAGE Rank2Types #-}

module LinguaQuanta.Quip.Transformers
  ( TofRule(..)
  , applyTransformer
  , elimCtrlsTransformer
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import Data.Algebra.Boolean (xor)
import LinguaQuanta.Quip.Quipper (QuipCirc(..))
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
  , gate_E_at
  , gate_H_at
  , gate_iX_at
  , gate_omega_at
  , gate_S_at
  , gate_T_at
  , gate_V_at
  , gate_W_at
  , gate_X_at
  , gate_Y_at
  , gate_Z_at
  , global_phase
  , global_phase_anchored
  , identity_transformer
  , qmultinot_at
  , qnot_at
  , reverse_imp_if
  , swap_at
  , transform_generic
  , without_controls_if
  )
import Quipper.Internal.Monad
  ( named_gate_qulist
  , named_rotation_qulist
  )
import Quipper.Internal.Transformer (B_Endpoint)
import Quipper.Libraries.GateDecompositions
  ( cc_iX_plain_at
  , cc_iX_S_at
  , cH_AMMR_at
  , controlled_E_at
  , controlled_iX_at
  , controlled_S_at
  , controlled_T_at
  , controlled_V_at
  , controlled_W_at
  , fredkin_at
  , toffoli_plain_at
  , toffoli_S_at
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

-- | A list of controls
type Endpoints = [Signed Endpoint]

-- | Indicates which Toffoli-like gate(s) should be use to decompose controls.
data TofRule = UseTof | UseCCIX | ElimTof deriving (Show, Eq)

-- | Takes as input a boolean flag and returns a Toffoli-like gate. If the flag
-- is not raised, then a Toffoli gate is returned. Otherwise, if the flag is
-- raised, then an optimal decomposition of doubly controlled iX gate is
-- returned. In other words, raising the flag eliminates Toffoli gates.
with_combined_controls_tof :: TofRule -> Int
                                      -> [Signed Endpoint] -> CtrlOp a
                                      -> Circ a
with_combined_controls_tof UseTof  = with_combined_controls toffoli_plain_at
with_combined_controls_tof UseCCIX = with_combined_controls cc_iX_plain_at
with_combined_controls_tof ElimTof = with_combined_controls cc_iX_S_at

-------------------------------------------------------------------------------
-- * Transformer to Eliminate Controls.

-- | Represents a list of classical and/or quantum controls.
type CtrlList = Ctrls Qubit Bit

-- | Represents a circuit returned by a transformer.
type ElimCtrlsRv = Circ ([Qubit], [Qubit], CtrlList)

-- | The signature of a rotational circuit.
type ElimCtrlSig = [Qubit] -> [Qubit] -> CtrlList -> ElimCtrlsRv

-- | Implements elimCtrls for QGates.
elimCtrlsQGate :: TofRule -> Bool -> Int
                          -> [Qubit] -> CtrlList -> Circ a
                          -> ElimCtrlsRv
elimCtrlsQGate tofRule ncf n ins ctrls op =
    without_controls_if ncf $
        with_combined_controls_tof tofRule n ctrls $ \ctrls' -> do
            op `controlled` ctrls'
            return (ins, [], ctrls)

-- | Implements elimCtrls for single qubit QGates whose controlled form:
-- 1. Does not appear in OpenQASM 2 as a primitive.
-- 2. Does appear in Quipper.Libraries.GateDecompositions.
-- Examples include: E, iX, S, T, and V gates.
elimCtrlsBr :: TofRule -> Bool -> Bool
                       -> Qubit -> CtrlList
                       -> (Qubit -> Circ ())
                       -> (Qubit -> Signed Qubit -> Circ ())
                       -> ElimCtrlsRv
elimCtrlsBr tofRule inv ncf q ctrls op cop =
    without_controls_if ncf $ do
        if null ctrls
        then (reverse_imp_if inv op) q
        else with_combined_controls_tof tofRule 1 ctrls $ \[c] -> do
            (reverse_imp_if inv cop) q c
        return ([q], [], ctrls)

-- | Implements elimCtrls for the swap QGate (a special case).
elimCtrlsSwap :: TofRule -> Bool -> Bool
                         -> Qubit -> Qubit -> CtrlList
                         -> ElimCtrlsRv
elimCtrlsSwap tofRule elimCSwap ncf q0 q1 ctrls =
    without_controls_if ncf $ do
        if elimCSwap
        then with_combined_controls_tof tofRule 1 ctrls $ \ctrls' -> do
            case ctrls' of
                []  -> swap_at q0 q1 
                [c] -> fredkin_at q0 q1 c
            return ([q0, q1], [], ctrls)
        else elimCtrlsQGate tofRule ncf 1 [q0, q1] ctrls $ swap_at q0 q1

-- | Implements elimCtrls for the H QGate (a special case).
elimCtrlsH :: TofRule -> Bool -> Bool -> Qubit -> CtrlList -> ElimCtrlsRv
elimCtrlsH tofRule elimCH ncf q ctrls =
    without_controls_if ncf $ do
        if elimCH
        then with_combined_controls_tof tofRule 1 ctrls $ \ctrls' -> do
            case ctrls' of
                []  -> gate_H_at q
                [c] -> cH_AMMR_at q c
            return ([q], [], ctrls)
        else elimCtrlsQGate tofRule ncf 1 [q] ctrls $ gate_H_at q

-- | Implements elimCtrls for the X QGate (a special case).
elimCtrlsX :: TofRule -> Bool -> Qubit -> CtrlList -> ElimCtrlsRv
elimCtrlsX tofRule ncf q ctrls =
    without_controls_if ncf $ do
        if tofRule == ElimTof
        then with_combined_controls_tof tofRule 2 ctrls $ \ctrls' -> do
            case ctrls' of
                []       -> gate_X_at q
                [c]      -> gate_X_at q `controlled` c
                [c1, c2] -> toffoli_S_at q c1 c2
            return ([q], [], ctrls)
        else elimCtrlsQGate tofRule ncf 2 [q] ctrls $ gate_X_at q

-- | Implements elimCtrls for the omega QGate (a special case).
elimCtrlsOmega :: TofRule -> Bool -> Bool -> Qubit -> CtrlList -> ElimCtrlsRv
elimCtrlsOmega tofRule inv ncf q ctrls =
    without_controls_if ncf $ do
        if null ctrls
        then (reverse_imp_if inv gate_omega_at) q
        else with_combined_controls_tof tofRule 1 ctrls $ \[c] -> do
            (reverse_imp_if inv $ global_phase 0.25) `controlled` c
        return ([q], [], ctrls)

-- | Implements elimCtrls for the W QGate (a special case).
elimCtrlsW :: TofRule -> Bool -> Bool
                      -> Qubit -> Qubit -> CtrlList
                      -> ElimCtrlsRv
elimCtrlsW tofRule inv ncf q0 q1 ctrls =
    without_controls_if ncf $ do
        if null ctrls
        then (reverse_imp_if inv gate_W_at) q0 q1
        else with_combined_controls_tof tofRule 1 ctrls $ \[c] -> do
            (reverse_imp_if inv controlled_W_at) q0 q1 c
        return ([q0, q1], [], ctrls)

-- | Implements elimCtrls for user-defined QGates.
elimCtrlsUserQGate :: String -> Bool -> Bool -> ElimCtrlSig
elimCtrlsUserQGate name inv ncf ins gens [] =
    without_controls_if ncf $ do
        named_gate_qulist name inv ins gens
        return (ins, gens, [])
elimCtrlsUserQGate name _ _ _ _ _ = error $ errMsg ++ name ++ "."
    where errMsg = "elimCtrlsTransformer: Cannot control user-defined gate "

-- | Implements elimCtrls for QRot gates.
elimCtrlsRotGate :: TofRule -> String -> Bool -> Double -> Bool -> ElimCtrlSig
elimCtrlsRotGate _ name inv ts ncf ins gens [] =
    without_controls_if ncf $ do
        named_rotation_qulist name inv ts ins gens
        return (ins, gens, [])
elimCtrlsRotGate tofRule name inv ts ncf ins gens ctrls =
    without_controls_if ncf $
        with_combined_controls_tof tofRule 1 ctrls $ \[c] -> do
            if name == "exp(-i%Z)" || name == "R(2pi/%)"
            then do
                named_rotation_qulist name inv ts ins gens `controlled` c
                return ()
            else do
                applyRot False
                qmultinot_at ins `controlled` c
                applyRot True
                qmultinot_at ins `controlled` c
                return ()
            return (ins, gens, ctrls)
    where halfTs = ts / 2
          applyRot i = named_rotation_qulist name (xor inv i) halfTs ins gens

-- | An anchor used by a GPhase gate.
type Anchor = B_Endpoint Qubit Bit

-- | Implements elimCtrls for GPhase gates.
elimCtrlsGPhase :: TofRule -> Double -> Bool -> [Anchor]
                           -> CtrlList -> Circ CtrlList
elimCtrlsGPhase tofRule ts ncf ins ctrls =
    without_controls_if ncf $
        with_combined_controls_tof tofRule 2 ctrls $ \ctrls' -> do
            global_phase_anchored ts ins `controlled` ctrls'
            return ctrls

-- | Quipper transformer to eliminate all controls, except for those supported
-- by OpenQASM 3. Supported controlled gates include:
-- 1. Controlled QGates: C(X), CC(X), C(Y), C(Z), C(swap), C(H), C(U)
-- 2. Controlled QRots: C(rX), C(rY), C(rZ)
-- If the tofRule flag is set, then CC(X) gates are also eliminated. Similarly,
-- elimCH eliminates C(H) gates and elimCSwap eliminates C(Swap) gates. Note
-- that the OpenQASM 2 gate P is a singly controlled phase gate.
elimCtrlsTransformer :: TofRule -> Bool -> Bool -> Transformer Circ Qubit Bit
-- Controllable QGates in OpenQASM 2.
elimCtrlsTransformer tofRule _ _ (T_QGate "multinot" _ 0 _ ncf f) = f $
    \ins [] ctrls -> elimCtrlsQGate tofRule ncf 2 ins ctrls $ qmultinot_at ins
elimCtrlsTransformer tofRule _ _ (T_QGate "not" 1 0 _ ncf f) = f $
    \[q] [] ctrls -> elimCtrlsX tofRule ncf q ctrls
elimCtrlsTransformer tofRule _ _ (T_QGate "X" 1 0 _ ncf f) = f $
    \[q] [] ctrls -> elimCtrlsX tofRule ncf q ctrls
elimCtrlsTransformer tofRule _ _ (T_QGate "Y" 1 0 _ ncf f) = f $
    \[q] [] ctrls -> elimCtrlsQGate tofRule ncf 1 [q] ctrls $ gate_Y_at q
elimCtrlsTransformer tofRule _ _ (T_QGate "Z" 1 0 _ ncf f) = f $
    \[q] [] ctrls -> elimCtrlsQGate tofRule ncf 1 [q] ctrls $ gate_Z_at q
elimCtrlsTransformer tofRule _ elimCSwap (T_QGate "swap" 2 0 _ ncf f) = f $
    \[q0, q1] [] ctrls -> elimCtrlsSwap tofRule elimCSwap ncf q0 q1 ctrls
elimCtrlsTransformer tofRule elimCH _ (T_QGate "H" 1 0 _ ncf f) = f $
    \[q] [] ctrls -> elimCtrlsH tofRule elimCH ncf q ctrls
-- Non-Controllable QGates in OpenQASM 2 (GateDecomposition).
elimCtrlsTransformer tofRule _ _ (T_QGate "E" 1 0 inv ncf f) = f $
    \[q] [] ctrls -> let op  = gate_E_at
                         cop = controlled_E_at
                     in elimCtrlsBr tofRule inv ncf q ctrls op cop
elimCtrlsTransformer tofRule _ _ (T_QGate "iX" 1 0 inv ncf f) = f $
    \[q] [] ctrls -> let op  = gate_iX_at
                         cop = controlled_iX_at
                     in elimCtrlsBr tofRule inv ncf q ctrls op cop
elimCtrlsTransformer tofRule _ _ (T_QGate "S" 1 0 inv ncf f) = f $
    \[q] [] ctrls -> let op  = gate_S_at
                         cop = controlled_S_at
                     in elimCtrlsBr tofRule inv ncf q ctrls op cop
elimCtrlsTransformer tofRule _ _ (T_QGate "T" 1 0 inv ncf f) = f $
    \[q] [] ctrls -> let op  = gate_T_at
                         cop = controlled_T_at
                     in elimCtrlsBr tofRule inv ncf q ctrls op cop
elimCtrlsTransformer tofRule _ _ (T_QGate "V" 1 0 inv ncf f) = f $
    \[q] [] ctrls -> let op  = gate_V_at
                         cop = controlled_V_at
                     in elimCtrlsBr tofRule inv ncf q ctrls op cop
-- Non-Controllable QGates in OpenQASM 2 (Special Cases).
elimCtrlsTransformer tofRule _ _ (T_QGate "W" 2 0 inv ncf f) = f $
    \[q0, q1] [] ctrls -> elimCtrlsW tofRule inv ncf q0 q1 ctrls
elimCtrlsTransformer tofRule _ _ (T_QGate "omega" 1 0 inv ncf f) = f $
    \[q] [] ctrls -> elimCtrlsOmega tofRule inv ncf q ctrls
-- User-Defined QGates.
elimCtrlsTransformer _ _ _ (T_QGate name _ _  inv ncf f) = f $
    \ins gens ctrls -> elimCtrlsUserQGate name inv ncf ins gens ctrls
-- QRots and GPhase Gates.
elimCtrlsTransformer tofRule _ _ (T_QRot name _ _ inv ts ncf f) = f $
    \ins gen ctrls -> elimCtrlsRotGate tofRule name inv ts ncf ins gen ctrls
elimCtrlsTransformer tofRule _ _ (T_GPhase ts ncf f) = f $
    \ins ctrls -> elimCtrlsGPhase tofRule ts ncf ins ctrls
-- Gates Without Controls.
elimCtrlsTransformer _ _ _ g = identity_transformer g
