-- | Abstract representation of OpenQasm 3 gates and their modifiers.

module Qasm.Gate
  ( Sign(..)
  , GateMod(..)
  , nullGateMod
  , negateMod
  , addCtrlsToMod
  , addNegCtrlsToMod
  , Gate(..)
  , invert
  , addCtrls
  , addNegCtrls
  , GateSummaryErr(..)
  , exprToGate
  ) where

import Qasm.Expression (ExprErr(..), toConstInt)
import Qasm.GateName (GateName, toGateName)
import Qasm.Language (Expr(..), GateOperand(..), GateExpr(..))

-------------------------------------------------------------------------------
-- * Modifiers and Update Functions.

-- | If true, then the gate is inverted.
type InverseFlag = Bool

-- | Represents the polarity of a control.
data Sign = Pos | Neg deriving (Show, Eq)

-- | Summarizes the modifiers applied to a gate.
data GateMod = GateMod InverseFlag [Sign] deriving (Show, Eq)

-- | Default initializes a gate modifier (no controls and not inverted).
nullGateMod :: GateMod
nullGateMod = GateMod False []

-- | Negates the inversion flag of a gate modifier. The positive and negative
-- controls are left unchanged.
negateMod :: GateMod -> GateMod
negateMod (GateMod inv ctrls) = GateMod (not inv) ctrls

-- | Consumes a count (n), a control polarity (sign), and a gate modifier.
-- Returns a new gate modifier, with n copies of sign prepended to the control
-- list, and the inverse flag left unchanged.
addSignsToMod :: Sign -> Int -> GateMod -> GateMod
addSignsToMod sign n (GateMod inv ctrls) = GateMod inv updated
    where updated = take n (repeat sign) ++ ctrls

-- Equivalent to (addSignsToMod Pos).
addCtrlsToMod :: Int -> GateMod -> GateMod
addCtrlsToMod = addSignsToMod Pos

-- Equivalent to (addSignsToMod Neg).
addNegCtrlsToMod :: Int -> GateMod -> GateMod
addNegCtrlsToMod = addSignsToMod Neg

-------------------------------------------------------------------------------
-- * Gates and Decorator Functions.

-- | Abstract representation of a gate: name, parameters, operands, and mods.
data Gate = NamedGate GateName [Expr] [GateOperand] GateMod
          | GPhaseGate Expr [GateOperand] GateMod
          deriving (Show, Eq)

-- | Consumes a modifier update function (f) and a gate. Returns the gate
-- obtained by applying f to the underlying gate modifier.
applyToMod :: (GateMod -> GateMod) -> Gate -> Gate
applyToMod f (NamedGate name params operands mod)
    = NamedGate name params operands (f mod)
applyToMod f (GPhaseGate param operands mod)
    = GPhaseGate param operands (f mod)

-- Equivalent to applyToMod negateMod.
invert :: Gate -> Gate
invert = applyToMod negateMod

-- | Equivalent to applyToMod (addCtrlsToMod n).
addCtrls :: Int -> Gate -> Gate
addCtrls n = applyToMod (addCtrlsToMod n)

-- | Equivalent to applyToMod (addNegCtrlsToMod n).
addNegCtrls :: Int -> Gate -> Gate
addNegCtrls n gate = applyToMod (addNegCtrlsToMod n) gate

-------------------------------------------------------------------------------
-- * Gate Summarization.

data GateSummaryErr = NonConstParam ExprErr Expr
                    | NonPosParam Int Expr
                    deriving (Show, Eq)

type GateEval = Either (Int, Gate) GateSummaryErr

-- | Consumes a gate update function (f), and a gate expression (expr). If expr
-- evaluates successfully to (n, gate), then returns (n, f gate). Otherwise, a
-- gate summarization error is returned to describe the failure.
tryUpdate :: (Gate -> Gate) -> GateExpr -> GateEval
tryUpdate f gateExpr =
    case exprToGate gateExpr of
        Left (n, gate) -> Left (n, f gate)
        Right err      -> Right err

-- | Consumes an expression (expr). If expr evaluates to a constant, positive
-- integer n, then n is returned. Otherwise, a gate summarization error is
-- returned to describe the failure.
tryParseParam :: Expr -> Either Int GateSummaryErr
tryParseParam expr =
    case toConstInt expr of
        Left n    -> if n > 0 then Left n else Right (NonPosParam n expr)
        Right err -> Right (NonConstParam err expr)

-- Consumes a gate update function parameterized by an integer (f), a gate
-- expression (gateExpr), and a parameter expression (paramExpr). If gateExpr
-- evaluates successfully to (n, gate) and paramExpr evaluates successfully to
-- m, then (n, f m gate) is returned. Otherwise, a gate summarization error is
-- returned to descrie the first failure.
tryParamUpdate :: (Int -> Gate -> Gate) -> GateExpr -> Expr -> GateEval
tryParamUpdate f gateExpr paramExpr =
    case tryParseParam paramExpr of
        Left n    -> tryUpdate (f n) gateExpr
        Right err -> Right err

-- Evaluates a gate expression as a tuple (n, gate) where gate is the specified
-- gate and n is the number of applications of gate. If summarization fails,
-- then a gate summarization error is returned to describe the first failure.
exprToGate :: GateExpr -> GateEval
exprToGate (NamedGateOp name params operands) = Left (1, gate)
    where gate = NamedGate (toGateName name) params operands nullGateMod
exprToGate (GPhaseOp param operands) = Left (1, gate)
    where gate = GPhaseGate param operands nullGateMod
exprToGate (CtrlMod Nothing gate)        = tryUpdate (addCtrls 1) gate
exprToGate (CtrlMod (Just expr) gate)    = tryParamUpdate addCtrls gate expr
exprToGate (NegCtrlMod Nothing gate)     = tryUpdate (addNegCtrls 1) gate
exprToGate (NegCtrlMod (Just expr) gate) = tryParamUpdate addNegCtrls gate expr
exprToGate (InvMod gate)                 = tryUpdate invert gate
exprToGate (PowMod expr gate) =
    case tryParseParam expr of
        Left m -> case exprToGate gate of
            Left (n, gate) -> Left (n + m, gate)
            Right err      -> Right err
        Right err -> Right err
