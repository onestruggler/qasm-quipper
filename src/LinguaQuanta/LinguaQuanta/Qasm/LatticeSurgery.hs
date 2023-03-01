-- | Functions to prepare OpenQASM files for lattice surgery compilation.

module LinguaQuanta.Qasm.LatticeSurgery
  ( LscGateErr(..)
  , lscRewriteGate
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.Gate
  ( Gate(..)
  , isInverted
  , isControlled
  , nullGateMod
  )
import LinguaQuanta.Qasm.GateName
  ( GateName(..)
  , isUGate
  )
import LinguaQuanta.Qasm.Language (Expr)
import LinguaQuanta.Qasm.Operand (Operand)

-------------------------------------------------------------------------------
-- * Predicates to Classify Gates.

-- | Returns true if the gate is implemented by the lattice surgey compiler.
isLscGate :: GateName -> Bool
isLscGate GateX   = True
isLscGate GateCX  = True
isLscGate GateZ   = True
isLscGate GateCZ  = True
isLscGate GateH   = True
isLscGate GateS   = True
isLscGate GateSdg = True
isLscGate GateT   = True
isLscGate GateTdg = True
isLscGate GateRX  = True
isLscGate GateCRX = True
isLscGate GateRZ  = True
isLscGate GateCRZ = True
isLscGate _       = False

-- | Returns true if the gate is a no-op with respect to compilation.
isLscNoop :: GateName -> Bool
isLscNoop GateQuipOmega = True
isLscNoop GateID        = True
isLscNoop _             = False

-- | Returns true if a gate requires special decomposition for support from the
-- lattice surgery compiler.
lscRequiresDecomp :: GateName -> Bool
lscRequiresDecomp GateCCX   = True
lscRequiresDecomp GateCSwap = True
lscRequiresDecomp GateRY    = True
lscRequiresDecomp GateCRY   = True
lscRequiresDecomp GateQuipW = True
lscRequiresDecomp GateCH    = True
lscRequiresDecomp _         = False

-------------------------------------------------------------------------------
-- * Helper Functions to Rewrite Gates.

-- | Decomposes an unmodified Swap gate into a sequence of CX gates.
rewriteSwap :: [Operand] -> [Gate]
rewriteSwap [a, b] = [cx a b, cx b a, cx a b]
    where cx x y = NamedGate GateCX [] [x, y] nullGateMod

-- | Decomposes an unmodified CY gate into a sequence of CX and S gates.
rewriteCY :: [Operand] -> [Gate]
rewriteCY [c, t] = [mk GateSdg [t], mk GateCX [c, t], mk GateS [t]]
    where mk name ops = NamedGate name [] ops nullGateMod

-------------------------------------------------------------------------------
-- * Preprocessing for Lattice Surgery Compilation.

-- | The description of a named gate without modifiers.
type NamedGateDesc = (GateName, [Expr], [Operand])

-- | Errors to explain why a program is not amenable to lattice surgery.
data LscGateErr = UnexpectedInvMod
                | UnexpectedCtrlMod
                | UnknownCompilation String
                | UnsupportedCompilation GateName
                deriving (Show, Eq)

-- | Takes the description (name, args, ops, _) of a named gate without
-- modifiers. If the gate can be supported by the lattice surgery compiler
-- (LSC), then a list of LSC-compliant gates are returned to implement the
-- operation. Otherwise, an error is returned explaining why the LSC does not
-- support the gate. For more details, see lscRewriteGate.
lscRewriteNamed :: NamedGateDesc -> Either [Gate] LscGateErr
lscRewriteNamed (UserDefined name, _, _) = Right $ UnknownCompilation name
lscRewriteNamed (name, args, ops)
    -- General error cases.
    | isUGate name           = Right $ UnsupportedCompilation name
    | lscRequiresDecomp name = Right $ UnsupportedCompilation name
    -- Simple cases: identities, noops, etc.
    | isLscNoop name = Left []
    | isLscGate name = Left [mk name]
    -- Unsupported gates with tractable rewrites.
    | name == GateCY   = Left $ rewriteCY ops
    | name == GateSX   = Left [mk GateH, mk GateSdg, mk GateH]
    | name == GateSwap = Left $ rewriteSwap ops
    -- Gates are uncontrolled and translation is correct up to a global phase.
    | name == GateY      = Left [mk GateX, mk GateZ]
    | name == GateQuipE  = Left [mk GateH, mk GateS, mk GateS, mk GateS]
    | name == GateQuipIX = Left [mk GateX]
    | name == GateP      = Left [mk GateRZ]
    | name == GateCP     = Left [mk GateCRZ]
    | name == GatePhase  = Left [mk GateRZ]
    | name == GateCPhase = Left [mk GateCRZ]
    -- Helper methods.
    where mk name = NamedGate name args ops nullGateMod

-- | Takes as input an OpenQASM gate. If the gate can be supported by the
-- lattice surgery compiler (LSC), then a list of LSC-compliant gates are
-- returned to implement the operation. Otherwise, an error is returned
-- explaining why the LSC does not support the gate. Possible errors include:
-- 1. Inversions: If the gate has inversion modifiers, then UnexpectedInvMod is
--    returned. To resolve this error, apply elim_invs.
-- 2. Controls: If the gate has a control modifier, then UnexpectedCtrlMod is
--    returned. To resolve this error, apply elim_ctrls.
-- 3. Toffoli Gates: If there are Toffoli gates in the circuit (including the
--    CSwap, W, and CH gates), then UnknownCompilation is returned. To resolve
--    this error, apply elim_ctrls with all decompositions enabled.
-- 4. User-Defined Gates: If there is a user-defined gate in the circuits, and
--    the gate cannot be inlined, then an UnknownCompilation error is returned.
-- 5. U-Gates: If there is a U-gate in the circuit, then an
--    UnsupportedCompilation error is returned. To resolve this error, perform
--    a round translation to eliminate all U-gate.
-- 6. Y-Rotations: If there is a Y-rotation in the circuit, then an
--    UnsupportedCompilation error is returned. To resolve this error, perform
--    a round translation to eliminate all Y-rotations.
lscRewriteGate :: Gate -> Either [Gate] LscGateErr
lscRewriteGate gate
    | isInverted gate   = Right UnexpectedInvMod
    | isControlled gate = Right UnexpectedCtrlMod
    | otherwise         = case gate of
        GPhaseGate _   _    _     -> Left []
        NamedGate name args ops _ -> lscRewriteNamed (name, args, ops)
