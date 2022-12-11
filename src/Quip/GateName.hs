-- | Named gates and associated data.

module Quip.GateName
  ( NamedOp(..)
  , GateName(..)
  , RotName(..)
  , toGateName
  , toRotName
  ) where

-------------------------------------------------------------------------------
-- * Gate Name Declarations.

-- | A named operator is a collection of unitary operators names, with optional
-- support for user-defined operators.
class NamedOp a where
    -- | Returns true if a gate is user-defined.
    isUserDefined :: a -> Bool
    -- | Returns the textual representation of a gate.
    printGate :: a -> String

-- | Assigns a unique type to each standard named gate in Quipper.
data GateName = GateX
              | GateY
              | GateZ
              | GateH
              | GateSwap
              | GateV
              | GateS
              | GateT
              | GateIX
              | GateOmega
              | GateW
              | GateE
              | UserDefinedGate String
              deriving (Show, Eq)

instance NamedOp GateName where
    -- | isUserDefined :: a -> Bool
    isUserDefined (UserDefinedGate _) = True
    isUserDefined _                   = False
    -- | printGate :: a -> String
    printGate GateX                 = "X"
    printGate GateY                 = "Y"
    printGate GateZ                 = "Z"
    printGate GateH                 = "H"
    printGate GateSwap              = "SWAP"
    printGate GateV                 = "V"
    printGate GateS                 = "S"
    printGate GateT                 = "T"
    printGate GateIX                = "iX"
    printGate GateOmega             = "Omega"
    printGate GateW                 = "W"
    printGate GateE                 = "E"
    printGate (UserDefinedGate str) = str

-- | Assigns a unique type to each standard rotation gate in Quipper.
--
-- Note: Both RotExpZ and RotZ are both e^(-iZt), up to a global phase. While
-- global phase is unobservation, the difference in phase is observable when
-- both RotExpZ and RotZ are controlled.    
data RotName = RotExpZ
             | RotZ
             | UserDefinedRot String
             deriving (Show, Eq)

instance NamedOp RotName where
    -- | isUserDefined :: a -> Bool
    isUserDefined (UserDefinedRot _) = True
    isUserDefined _                  = False
    -- | printGate :: a -> String
    printGate RotExpZ              = "exp(-i%Z)"
    printGate RotZ                 = "R(2pi/%)"
    printGate (UserDefinedRot str) = str

-------------------------------------------------------------------------------
-- * Name Analysis Functions.

-- | Interprets a string as a standard Quipper gate name.
toGateName :: String -> GateName
toGateName "not"   = GateX
toGateName "X"     = GateX
toGateName "Y"     = GateY
toGateName "Z"     = GateZ
toGateName "H"     = GateH
toGateName "SWAP"  = GateSwap
toGateName "V"     = GateV
toGateName "S"     = GateS
toGateName "T"     = GateT
toGateName "iX"    = GateIX
toGateName "Omega" = GateOmega
toGateName "W"     = GateW
toGateName "E"     = GateE
toGateName str     = UserDefinedGate str

-- | Interprets a string as a standard Quipper rotation name.
toRotName :: String -> RotName
toRotName "exp(-i%Z)" = RotExpZ
toRotName "R(2pi/%)"  = RotZ
toRotName str         = UserDefinedRot str
