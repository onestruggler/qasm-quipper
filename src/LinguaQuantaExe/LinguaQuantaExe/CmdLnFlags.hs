-- | Shared flags between command-line parsers.

{-# LANGUAGE DeriveDataTypeable #-}

module LinguaQuantaExe.CmdLnFlags
  ( QuipVizFormat(..)
  , def
  , elimGateFlags
  , outFlags
  , quipVizFlags
  , srcFlags
  , legacyFlags
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import Data.List
  ( concat
  , intersperse
  )
import System.Console.CmdArgs
  ( Data
  , Typeable
  , (&=)
  , def
  , typ
  , help
  , typFile
  )

-------------------------------------------------------------------------------
-- * Input/Output Flags.

-- | Returns the flags for the --src argument. The default value is taken as an
-- argument, since flags are impure.
srcFlags :: String -> String
srcFlags x = x &= help "Input source (defaults to stdin)."
               &= typFile

-- | Returns the flags for the --out argument. The default value is taken as an
-- argument, since flags are impure.
outFlags :: String -> String
outFlags x = x &= help "Output destination (defaults to stdout)."
               &= typFile

-------------------------------------------------------------------------------
-- * Gate Set Flags.

-- | Takes as input the name of a gate. Returns the flags for the --elim_{gate}
-- argument. The default value is taken as an argument, since flags are impure.
elimGateFlags :: String -> Bool -> Bool
elimGateFlags gate x = x &= help helpMsg
    where helpMsg = "Decompose all " ++ gate ++ " gates."

-------------------------------------------------------------------------------
-- * Output Format Flags.

-- | Enumeration of supported Quipper formats.
data QuipVizFormat = PDF
                   | PS
                   | EPS
                   | GateCount
                   deriving (Show, Eq, Data, Typeable, Enum)

-- | Returns the flags for the --viz argument. The default value is taken as an
-- argument, since flags are impure.
quipVizFlags :: QuipVizFormat -> QuipVizFormat
quipVizFlags x = x &= help ("Options: " ++ names)
                   &= typ "FORMAT"
    where elist = enumFrom $ toEnum 0 :: [QuipVizFormat]
          slist = map show elist
          names = concat $ intersperse ", " slist

-- | Returns the flags for the --legacy argument. The default value is taken as
-- an argument, since flags are impure.
legacyFlags :: Bool -> Bool
legacyFlags x = x &= help "Generate OpenQASM 2.0 output."
