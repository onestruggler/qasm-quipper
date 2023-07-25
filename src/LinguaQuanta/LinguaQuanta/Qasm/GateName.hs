-- | Named gates and associated data.

module LinguaQuanta.Qasm.GateName where

-------------------------------------------------------------------------------
-- * Gate Name Declarations.

-- | Assigns a unique type to each named gate in OpenQASM 3.
data GateName = UserDefined String
              -- Non-Rotational OpenQASM Gates:
              | GateX
              | GateCX
              | GateCCX
              | GateY
              | GateCY
              | GateZ
              | GateCZ
              | GateH
              | GateCH
              | GateSwap
              | GateCSwap
              | GateS
              | GateSdg
              | GateSX
              | GateT
              | GateTdg
              | GateID
              -- Non-Rotational Quipper Integration Gates:
              | GateQuipIX
              | GateQuipOmega
              | GateQuipE
              | GateQuipW
              -- Rotational OpenQASM Gates:
              | GateRX
              | GateCRX
              | GateRY
              | GateCRY
              | GateRZ
              | GateCRZ
              | GateP
              | GateCP
              | GatePhase
              | GateCPhase
              | GateU
              | GateCU
              | GateU1
              | GateU2
              | GateU3
              deriving (Show, Eq)

-------------------------------------------------------------------------------
-- * Name Analysis Functions.

-- | Interprets a string as an OpenQASM 3 gate name.
toGateName :: String -> GateName
toGateName "x"          = GateX
toGateName "cx"         = GateCX
toGateName "CX"         = GateCX
toGateName "ccx"        = GateCCX
toGateName "y"          = GateY
toGateName "cy"         = GateCY
toGateName "z"          = GateZ
toGateName "cz"         = GateCZ
toGateName "h"          = GateH
toGateName "ch"         = GateCH
toGateName "swap"       = GateSwap
toGateName "cswap"      = GateCSwap
toGateName "s"          = GateS
toGateName "sdg"        = GateSdg
toGateName "sx"         = GateSX
toGateName "t"          = GateT
toGateName "tdg"        = GateTdg
toGateName "id"         = GateID
toGateName "quip_ix"    = GateQuipIX
toGateName "quip_omega" = GateQuipOmega
toGateName "quip_e"     = GateQuipE
toGateName "quip_w"     = GateQuipW
toGateName "rx"         = GateRX
toGateName "crx"        = GateCRX
toGateName "ry"         = GateRY
toGateName "cry"        = GateCRY
toGateName "rz"         = GateRZ
toGateName "crz"        = GateCRZ
toGateName "p"          = GateP
toGateName "cp"         = GateCP
toGateName "phase"      = GatePhase
toGateName "cphase"     = GateCPhase
toGateName "U"          = GateU
toGateName "cu"         = GateCU
toGateName "u1"         = GateU1
toGateName "u2"         = GateU2
toGateName "u3"         = GateU3
toGateName str          = UserDefined str

-- | Returns the number of parameters expected by a named gate. If nothing is
-- returned, then the number of parameters is user-defined.
toParamCount :: GateName -> Maybe Int
toParamCount (UserDefined _) = Nothing
toParamCount GateRX          = Just 1
toParamCount GateCRX         = Just 1
toParamCount GateRY          = Just 1
toParamCount GateCRY         = Just 1
toParamCount GateRZ          = Just 1
toParamCount GateCRZ         = Just 1
toParamCount GateP           = Just 1
toParamCount GateCP          = Just 1
toParamCount GatePhase       = Just 1
toParamCount GateCPhase      = Just 1
toParamCount GateU           = Just 3
toParamCount GateCU          = Just 4
toParamCount GateU1          = Just 1
toParamCount GateU2          = Just 2
toParamCount GateU3          = Just 3
toParamCount _               = Just 0

-- | Returns the number of operands consumed by a name gate (without controls).
-- If nothing is returned, then the number of operands is user-defined.
toOperandCount :: GateName -> Maybe Int
toOperandCount (UserDefined _) = Nothing
toOperandCount GateCX          = Just 2
toOperandCount GateCCX         = Just 3
toOperandCount GateCY          = Just 2
toOperandCount GateCZ          = Just 2
toOperandCount GateCH          = Just 2
toOperandCount GateSwap        = Just 2
toOperandCount GateCSwap       = Just 3
toOperandCount GateQuipW       = Just 2
toOperandCount GateCRX         = Just 2
toOperandCount GateCRY         = Just 2
toOperandCount GateCRZ         = Just 2
toOperandCount GateCP          = Just 2
toOperandCount GateCPhase      = Just 2
toOperandCount GateCU          = Just 2
toOperandCount _               = Just 1

-- | Returns true if a gate is self-inverse.
isSelfInverse :: GateName -> Bool
isSelfInverse GateX     = True
isSelfInverse GateCX    = True
isSelfInverse GateCCX   = True
isSelfInverse GateY     = True
isSelfInverse GateCY    = True
isSelfInverse GateZ     = True
isSelfInverse GateCZ    = True
isSelfInverse GateH     = True
isSelfInverse GateCH    = True
isSelfInverse GateSwap  = True
isSelfInverse GateCSwap = True
isSelfInverse GateID    = True
isSelfInverse GateQuipW = True
isSelfInverse _         = False

-- | Returns true if a gate is inverted by negating all parameters to the gate.
isParamInverse :: GateName -> Bool
isParamInverse GateRX     = True
isParamInverse GateCRX    = True
isParamInverse GateRY     = True
isParamInverse GateCRY    = True
isParamInverse GateRZ     = True
isParamInverse GateCRZ    = True
isParamInverse GateP      = True
isParamInverse GateCP     = True
isParamInverse _          = False

-- | Returns true if the gate is defined in the qelib1.inc library.
isQelib1Gate :: GateName -> Bool
isQelib1Gate GateX      = True
isQelib1Gate GateCX     = True
isQelib1Gate GateCCX    = True
isQelib1Gate GateY      = True
isQelib1Gate GateCY     = True
isQelib1Gate GateZ      = True
isQelib1Gate GateCZ     = True
isQelib1Gate GateH      = True
isQelib1Gate GateCH     = True
isQelib1Gate GateS      = True
isQelib1Gate GateSdg    = True
isQelib1Gate GateT      = True
isQelib1Gate GateTdg    = True
isQelib1Gate GateID     = True
isQelib1Gate GateRX     = True
isQelib1Gate GateRY     = True
isQelib1Gate GateRZ     = True
isQelib1Gate GateCRZ    = True
isQelib1Gate GatePhase  = True
isQelib1Gate GateCPhase = True
isQelib1Gate GateU1     = True
isQelib1Gate GateU2     = True
isQelib1Gate GateU3     = True
isQelib1Gate _          = False

-- | Returns true if the gate is defined in the bkpgate.inc library.
isBackportGate :: GateName -> Bool
isBackportGate GateSwap  = True
isBackportGate GateCSwap = True
isBackportGate GateSX    = True
isBackportGate GateCRX   = True
isBackportGate GateCRY   = True
isBackportGate GateP     = True
isBackportGate GateCP    = True
isBackportGate GateCU    = True
isBackportGate _         = False

-- | Returns true if the gate is defined in the quipgates.inc library.
isQuipperGate :: GateName -> Bool
isQuipperGate GateQuipIX    = True
isQuipperGate GateQuipOmega = True
isQuipperGate GateQuipE     = True
isQuipperGate GateQuipW     = True
isQuipperGate _             = False

-- | Returns true if the gate is some variation of the universal U-gate.
isUGate :: GateName -> Bool
isUGate GateU  = True
isUGate GateCU = True
isUGate GateU1 = True
isUGate GateU2 = True
isUGate GateU3 = True
isUGate _      = False
