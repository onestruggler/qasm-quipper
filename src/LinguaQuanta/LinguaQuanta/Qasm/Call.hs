-- | Functions for analzing and inlining function calls.

module LinguaQuanta.Qasm.Call (isLegacyCall) where

-------------------------------------------------------------------------------
-- * Call Analysis.

-- | Returns true if a call name is built into OpenQASM 2.0.
isLegacyCall :: String -> Bool
isLegacyCall "sin"  = True
isLegacyCall "cos"  = True
isLegacyCall "tan"  = True
isLegacyCall "exp"  = True
isLegacyCall "ln"   = True
isLegacyCall "sqrt" = True
isLegacyCall _      = False
