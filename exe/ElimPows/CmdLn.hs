-- | Command-line parser for power elimination tool.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module ElimPows.CmdLn
  ( ElimPowsTool(..)
  , getToolArgs
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuantaExe.CmdLnFlags
  ( def
  , outFlags
  , srcFlags
  )
import LinguaQuantaExe.CmdLnParser
  ( Data
  , Typeable
  , addModeAnnotations
  , parseCmdLnArgs
  )

-------------------------------------------------------------------------------
-- * Argument Data Type.

data ElimPowsTool = ElimPows { src :: String
                             , out :: String
                             } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

elimPowsMode :: ElimPowsTool
elimPowsMode = ElimPows { src = srcFlags def
                        , out = outFlags def
                        }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getToolArgs :: IO ElimPowsTool
getToolArgs = parseCmdLnArgs title desc ctors
    where title = "Powers Elimination Pass"
          desc  = "A command-line interface to inline OpenQASM pow modifiers."
          ctors = [elimPowsMode]
