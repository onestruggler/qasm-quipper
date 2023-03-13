-- | Functions to prepare OpenQASM files for lattice surgery compilation.

module LinguaQuanta.Qasm.LatticeSurgery
  ( LscGateErr(..)
  , MergedVarMap
  , addCDecl
  , addQDecl
  , lookupCtorOperand
  , lookupCVar
  , lookupOperand
  , lookupQVar
  , lscRewriteGate
  , makeMergedVarMap
  , toCDecl
  , toMergedSize
  , toQDecl
  , updateExpr
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import LinguaQuanta.Either
  ( expandLeft
  , leftMap
  )
import LinguaQuanta.Maybe (branchJust)
import LinguaQuanta.Qasm.Expression
  ( applyBinaryOp
  , applyUnaryOp
  , zero
  )
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
import LinguaQuanta.Qasm.Language
  ( Expr(..)
  , GateOperand(..)
  )
import LinguaQuanta.Qasm.Operand (Operand(..))

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
-- * Preprocessing to merge all variables into a single register.

-- | Merges variables of a given type into a single register. Retains the name
-- of the register, the current offset (number of cells), and a mapping from
-- variable names to offsets. For example, if v has offset n then v[i] has
-- offset (n + i).
data TypedVarMap = TypedVarMap String Int (Map.Map String Int)

-- | Returns a valid TypedVarMap with the given register name and offset 0.
makeTypedVarMap :: String -> TypedVarMap
makeTypedVarMap reg = TypedVarMap reg 0 Map.empty

-- | Takes as input a TypedVarMap, the name of a variable, and its size (by
-- convention, a scalar declaration has size 1). Returns the TypedVarDecl map
-- obtained by mapping the variable name to a region of the given size.
addDecl :: TypedVarMap -> String -> Int -> TypedVarMap
addDecl (TypedVarMap reg offset map) id size = TypedVarMap reg offset' map'
    where offset' = offset + size
          map'    = Map.insert id offset map

-- | Returns the register name and size for a TypedVarDecl.
toDecl :: TypedVarMap -> (String, Int)
toDecl (TypedVarMap reg offset _) = (reg, offset)

-- | Takes as input a TypedVarMap, the name of a variable, and an index into
-- the variable (by convention, a scalar has index 0). If the variable's name
-- has been added to the TypedVarMap, then the register name and index
-- associated with the cell are returned. Otherwise, nothing is returned.
lookupVar :: TypedVarMap -> String -> Int -> Maybe (String, Int)
lookupVar (TypedVarMap reg _ map) id idx =
    branchJust (Map.lookup id map) $ \offset -> Just (reg, offset + idx)

-- | Maintains a TypedVarMap for quantum variables, and a TypedVarMap for
-- classical variables.
data MergedVarMap = MergedVarMap TypedVarMap TypedVarMap

-- | Takes as input a name for a quantum register and a name for a classical
-- register. Initializes a MergedVarMap where the quantum and classical
-- registers have the given names, and both have offset 0.
makeMergedVarMap :: String -> String -> MergedVarMap
makeMergedVarMap qreg creg = MergedVarMap qvars cvars
    where qvars = makeTypedVarMap qreg
          cvars = makeTypedVarMap creg

-- | Takes as input a MergedVarMap, the name of a quantum variable, and its size
-- (by convention, a scalar declaration has size 1). Returns the MergedVarMap
-- obtained by mapping the variable name to a quantum region of the given size.
addQDecl :: MergedVarMap -> String -> Int -> MergedVarMap
addQDecl (MergedVarMap qvars cvars) id size = MergedVarMap qvars' cvars
    where qvars' = addDecl qvars id size

-- | addCDecl for classical variables.
addCDecl :: MergedVarMap -> String -> Int -> MergedVarMap
addCDecl (MergedVarMap qvars cvars) id size = MergedVarMap qvars cvars'
    where cvars' = addDecl cvars id size

-- | Returns the register name and size for the quantum register of a
-- MergedVarMap.
toQDecl :: MergedVarMap -> (String, Int)
toQDecl (MergedVarMap qvars _) = toDecl qvars

-- | toQDecl for classical registers.
toCDecl :: MergedVarMap -> (String, Int)
toCDecl (MergedVarMap _ cvars) = toDecl cvars

-- | Takes as input a MergedVarMap, the name of a quantum variable, and an
-- index into the variable (by convention, a scalar has index 0). If the
-- variable's name has been added to the quantum register of the MergedVarMap,
-- then the register name and index associated with the cell are returned.
-- Otherwise, nothing is returned.
lookupQVar :: MergedVarMap -> String -> Int -> Maybe (String, Int)
lookupQVar (MergedVarMap qvars _) id idx = lookupVar qvars id idx

-- | qLookup for classical entrties
lookupCVar :: MergedVarMap -> String -> Int -> Maybe (String, Int)
lookupCVar (MergedVarMap _ cvars) id idx = lookupVar cvars id idx

-------------------------------------------------------------------------------
-- * Preprocessing for operands when merging variables into a single register.

-- | Takes as input the size of a variable declaration (i.e., Nothing for a
-- scalar variable and (Just n) for an array with n cells). Returns the size
-- expected by MergedVarMap.
toMergedSize :: Maybe Int -> Int
toMergedSize = fromMaybe 1

-- | Takes as input a MergedVarMap and a quantum operand. If the operand can be
-- found in the quantum declaration map, then an equivalent operand using the
-- new register is returned. Otherwise, the name of the unknown declaration is
-- returned as an error message.
lookupOperand :: MergedVarMap -> Operand -> Either Operand String
lookupOperand map (Cell id idx) =
    case lookupQVar map id idx of
        Just (id', idx') -> Left $ Cell id' idx'
        Nothing          -> Right id
lookupOperand map (QRef id) = lookupOperand map (Cell id 0)

-- | Takes as input a MergedVarMap, a type constructor parameterized by a
-- quantum operand (e.g., for QInit0 or QTerm0), and an operand. If the operand
-- can be found in the quantum declaration map, then the constructor is applied
-- to an equivalent operand using the new register, and the resulting value is
-- returned. Otherwise, the name of the unknown declaration is returned as an
-- error message.
lookupCtorOperand :: MergedVarMap -> (Operand -> a) -> Operand -> Either a String
lookupCtorOperand map ctor op = expandLeft (lookupOperand map op) $
                                           \op' -> Left $ ctor op'

-------------------------------------------------------------------------------
-- * Preprocessing for exprs when merging variables into a single register.

-- | Takes as input a MergedVarMap and an identifier. First attempts to resolve
-- the identifier as a quantum variable. If this fails, then tries to resolve
-- the identifier as a classical variable. If either attempt fails, then the
-- ofset is returned. Otherwise, nothing is returned.
lookupExprId :: MergedVarMap -> String -> Either (String, Int) String
lookupExprId (MergedVarMap qvars cvars) id =
    case lookupVar qvars id 0 of
        Just x  -> Left x
        Nothing -> case lookupVar cvars id 0 of
            Just x  -> Left x
            Nothing -> Right id

-- | Takes as input a MergedVarMap and an unparsed gate operand. If the operand
-- can be found in the quantum declaration map, then an equivalent operand
-- using the new register is returned. Otherwise, the name of the unknown
-- declaration is returned as an error message.
lookupGateOperand :: MergedVarMap -> GateOperand -> Either GateOperand String
lookupGateOperand map (QReg id idx) =
    case lookupQVar map id 0 of
        Just (id', offset) -> expandLeft (updateExpr map idx) $
            \idx -> Left $ QReg id' $ Plus idx $ DecInt $ show offset
        Nothing -> Right id
lookupGateOperand map (QVar id) = lookupGateOperand map (QReg id zero)

-- | Takes as input a MergedVarMap and an OpenQASM expression. Returns the
-- expression obtained by rewriting every variable access as dictated by the
-- MergedVarMap (i.e., applying updateExpr twice will result in an error).
updateExpr :: MergedVarMap -> Expr -> Either Expr String
updateExpr map (Plus lhs rhs)   = applyBinaryOp (updateExpr map) Plus lhs rhs
updateExpr map (Minus lhs rhs)  = applyBinaryOp (updateExpr map) Minus lhs rhs
updateExpr map (Times lhs rhs)  = applyBinaryOp (updateExpr map) Times lhs rhs
updateExpr map (Div lhs rhs)    = applyBinaryOp (updateExpr map) Div lhs rhs
updateExpr map (Brack expr)     = applyUnaryOp (updateExpr map) Brack expr
updateExpr map (Negate expr)    = applyUnaryOp (updateExpr map) Negate expr
updateExpr _   Euler            = Left Euler
updateExpr _   Pi               = Left Pi
updateExpr _   Tau              = Left Tau
updateExpr _   lit@(DecInt _)   = Left lit
updateExpr _   lit@(DecFloat _) = Left lit
updateExpr map (QasmId id)      = updateExpr map (QasmCell id zero)
updateExpr map (Call str args)  =
    expandLeft (leftMap (updateExpr map) args) $
        \args' -> Left $ Call str args'
updateExpr map (QasmCell id idx) =
    expandLeft (lookupExprId map id) $
        \(id', offset) -> expandLeft (updateExpr map idx) $
            \idx' -> Left $ QasmCell id' $ Plus idx' $ DecInt $ show offset
updateExpr map (QasmMeasure gop) =
    expandLeft (lookupGateOperand map gop) $
        \gop' -> Left $ QasmMeasure gop'

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
