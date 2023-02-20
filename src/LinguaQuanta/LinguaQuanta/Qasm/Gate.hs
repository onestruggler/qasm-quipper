-- | Abstract representation of OpenQasm 3 gates and their modifiers.

module LinguaQuanta.Qasm.Gate
  ( Gate(..)
  , GateMod(..)
  , GateSummaryErr(..)
  , Operand(..)
  , Sign(..)
  , addCtrls
  , addCtrlsToMod
  , addNegCtrls
  , addNegCtrlsToMod
  , exprToGate
  , getCtrlList
  , hasInversionMod
  , invert
  , isInverted
  , negateMod
  , nullGateMod
  , validateGate
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.Expression
  ( ExprErr(..)
  , parseGateOperand
  , toConstInt
  )
import LinguaQuanta.Qasm.GateName
  ( GateName
  , toGateName
  , toParamCount
  , toOperandCount
  )
import LinguaQuanta.Qasm.Language
  ( Expr(..)
  , GateOperand(..)
  , GateExpr(..)
  )
import LinguaQuanta.Qasm.Operand (Operand(..))

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

-- | Takes as input a count (n), a polarity (sign), and a gate modifier.
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

-- | Returns true if the inversion modifier is active.
hasInversionMod :: GateMod -> Bool
hasInversionMod (GateMod inv _) = inv

-- | Returns a list of control modifiers.
getCtrlList :: GateMod -> [Sign]
getCtrlList (GateMod _ ctrls) = ctrls

-------------------------------------------------------------------------------
-- * Gates and Decorator Functions.

-- | Abstract representation of a gate: name, parameters, operands, and mods.
data Gate = NamedGate GateName [Expr] [Operand] GateMod
          | GPhaseGate Expr [Operand] GateMod
          deriving (Show, Eq)

-- | Takes as input a modifier update function (f) and a gate. Returns the gate
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

-- | Returns true if the gate is inverted.
isInverted :: Gate -> Bool
isInverted (NamedGate _ _ _ mod) = hasInversionMod mod
isInverted (GPhaseGate _ _ mod)  = hasInversionMod mod

-------------------------------------------------------------------------------
-- * Gate Summarization.

data GateSummaryErr = NonConstParam ExprErr Expr
                    | NonPosParam Int Expr
                    | BadArrIndex ExprErr
                    | UnexpectedParamCount Int Int
                    | UnexpectedOperandCount Int Int
                    deriving (Show, Eq)

type GateEval = Either (Int, Gate) GateSummaryErr

-- | Takesa as input a gate update function (f), and a gate expression (expr).
-- If expr evaluates successfully to (n, gate), then returns (n, f gate).
-- Otherwise, a gate summarization error is returned to describe the failure.
tryUpdate :: (Gate -> Gate) -> GateExpr -> GateEval
tryUpdate f gateExpr =
    case exprToGate gateExpr of
        Left (n, gate) -> Left (n, f gate)
        Right err      -> Right err

-- | Takes as input an expression (expr). If expr evaluates to a constant,
-- positive integer n, then n is returned. Otherwise, a gate summarization
-- error is returned to describe the failure.
tryParseParam :: Expr -> Either Int GateSummaryErr
tryParseParam expr =
    case toConstInt expr of
        Left n    -> if n > 0 then Left n else Right (NonPosParam n expr)
        Right err -> Right (NonConstParam err expr)

-- | Wrapper to parseGateOperand.
tryParseOperand :: GateOperand -> Either Operand GateSummaryErr
tryParseOperand gop =
    case parseGateOperand gop of
        Left op   -> Left op
        Right err -> Right $ BadArrIndex err

-- | Applies tryParseOperand to a list of GateOperands. The first error from
-- left-to-right is returned.
tryParseOperands :: [GateOperand] -> Either [Operand] GateSummaryErr
tryParseOperands [] = Left []
tryParseOperands (gop:gops) =
    case tryParseOperand gop of
        Right err -> Right err
        Left op   -> case tryParseOperands gops of
            Right err -> Right err
            Left ops  -> Left $ op:ops

-- | Takes as input a gate update function parameterized by an integer (f), 
-- gate expression (gateExpr), and a parameter expression (paramExpr). If
-- gateExpr evaluates successfully to (n, gate) and paramExpr evaluates
-- successfully to m, then (n, f m gate) is returned. Otherwise, a gate
-- summarization error is returned to descrie the first evaluation failure.
tryParamUpdate :: (Int -> Gate -> Gate) -> GateExpr -> Expr -> GateEval
tryParamUpdate f gateExpr paramExpr =
    case tryParseParam paramExpr of
        Left n    -> tryUpdate (f n) gateExpr
        Right err -> Right err

-- | Evaluates a gate expression as a tuple (n, gate) where gate is the
-- specified gate and n is the number of applications of gate. If summarization
-- fails, then a gate summarization error is returned to describe the first
-- evaluation failure.
exprToGate :: GateExpr -> GateEval
exprToGate (NamedGateOp nameStr params gops) =
    case tryParseOperands gops of
        Right err -> Right err
        Left  ops -> let name = toGateName nameStr
                         gate = NamedGate name params ops nullGateMod
                     in Left (0, gate)
exprToGate (GPhaseOp param gops) =
    case tryParseOperands gops of
        Right err -> Right err
        Left  ops -> let gate = GPhaseGate param ops nullGateMod
                     in Left (0, gate)
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

-------------------------------------------------------------------------------
-- * Gate Validation.

-- | Takes as input a parameter list (params) and an optional integer (n). If n
-- is provided, and the length of params is not n, then an UnexpectedParamCount
-- exception is returned. Otherwise, nothing is returned.
checkParamCt :: Maybe Int -> [Expr] -> Maybe GateSummaryErr
checkParamCt Nothing  _ = Nothing
checkParamCt (Just expect) params
    | actual == expect = Nothing
    | otherwise        = Just (UnexpectedParamCount actual expect)
    where actual = length params

-- | Takes as input a gate modifier (mod), an operand list (operands), and an
-- optional integer (n). If n is not provided, and the number of controls in
-- mod exceeds the number of operands, then an UnexpectedOperandCount expection
-- is returned. Likewise, if n is provided, and the number of controls plus n
-- does not equal the number of operands, then an UnexpectedOperandCount
-- expection is returned. Otherwise, nothing is returned.
checkOpCt :: Maybe Int -> GateMod -> [Operand] -> Maybe GateSummaryErr
checkOpCt Nothing  (GateMod _ ctrls) operands
    | actual >= expect = Nothing
    | otherwise        = Just (UnexpectedOperandCount actual expect)
    where actual = length operands
          expect = length ctrls + 1
checkOpCt (Just n) (GateMod _ ctrls) operands
    | actual == expect = Nothing
    | otherwise        = Just (UnexpectedOperandCount actual expect)
    where actual = length operands
          expect = length ctrls + n

-- | Returns an exception if either the parameter count or the operand count is
-- invalid for the given gate.
validateGate :: Gate -> Maybe GateSummaryErr
validateGate (NamedGate name params operands mods) =
    case checkParamCt (toParamCount name) params of
        Just err -> Just err
        Nothing  -> checkOpCt (toOperandCount name) mods operands
validateGate (GPhaseGate _ operands mods) = checkOpCt (Just 0) mods operands
