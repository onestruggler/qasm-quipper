-- | Shared flags between command-line parsers.

module CmdLnFlags
  ( srcFlags
  , outFlags
  , legacyFlags
  , inlinePowFlags
  , inlineInvFlags
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import System.Console.CmdArgs

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
-- * Pre/Post-Processing Flags.

-- | Returns the flags for the --legacy argument. The default value is taken as
-- an argument, since flags are impure.
legacyFlags :: Bool -> Bool
legacyFlags x = x &= help "Generate OpenQASM 2.0 output."

-- | Returns the flags for the --inlinepow argument. The default value is taken
-- as an argument, since flags are impure.
inlinePowFlags :: Bool -> Bool
inlinePowFlags x = x &= help "Inlines all pow modifiers."

-- | Returns the flags for the --inlineinv argument. The default value is taken
-- as an argument, since flags are impure.
inlineInvFlags :: Bool -> Bool
inlineInvFlags x = x &= help "Inlines all inv modifiers."
