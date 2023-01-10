-- | Functions to translate OpenQASM controls to Quipper controls.

module LinguaQuanta.QasmToQuip.Control
  ( extractCtrls
  , toQuipControl
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.Gate as Qasm
import LinguaQuanta.Quip.Gate as Quip

-------------------------------------------------------------------------------
-- * Extract Quipper Controls.

-- | Takes as input a Quipper wire (w) and an OpenQASM control modifier (sgn).
-- Returns a Quipper control for wire w of polarity sgn.
toQuipControl :: Quip.Wire -> Qasm.Sign -> Quip.Control
toQuipControl w Qasm.Pos = Quip.Pos w
toQuipControl w Qasm.Neg = Quip.Neg w

-- | Takes as input a list of Quipper wires (ws) and a list of OpenQASM control
-- modifiers (sgns). Splits ws into two lists ws' and ins such that ws=ws':in
-- with (length ws') == (length sgns). Returns a tuple (ins, ctrls) such that
-- (length ctrls) = (length ws') and (ctrls !! i) is (ws' !! i) promoted to a
-- Quipper control of polarity (sgns !! i).
--
-- For example, extractCtrlsImpl [1, 2, 3, 4, 5] [Pos, Neg, Pos] will return
-- the tuple ([4, 5], [Pos 1, Neg 2, Pos 3]).
--
-- Note: Requires that (length ws) >= (length sgns).
extractCtrlsImpl :: [Quip.Wire] -> [Qasm.Sign] -> ([Quip.Wire], [Quip.Control])
extractCtrlsImpl wires [] = (wires, [])
extractCtrlsImpl []    _  = error msg
    where msg = "OpenQASM gate with unexpected number of control modifiers."
extractCtrlsImpl (w:ws) (s:sgns) = (ins, ctrl:ctrls)
    where ctrl         = toQuipControl w s
          (ins, ctrls) = extractCtrlsImpl ws sgns

-- | Takes as input a list of Quipper wires and an OpenQASM gate modifier.
-- Partitions the wires into an input set and a control set, according to the
-- gate modifier. For more details, see the specifications for extractCtrlImpl.
extractCtrls :: [Quip.Wire] -> Qasm.GateMod -> ([Quip.Wire], [Quip.Control])
extractCtrls wires mod = extractCtrlsImpl wires $ getCtrlList mod
