-- | Shared flags between command-line parsers.

module LinguaQuantaExe.CmdLnFlags
  ( def
  , srcFlags
  , outFlags
  , legacyFlags
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import System.Console.CmdArgs
  ( (&=)
  , def
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
-- * Output Format Flags.

-- | Returns the flags for the --legacy argument. The default value is taken as
-- an argument, since flags are impure.
legacyFlags :: Bool -> Bool
legacyFlags x = x &= help "Generate OpenQASM 2.0 output."
