-- | Command-line parser for inversion elimination tool.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module ElimInvs.CmdLn
  ( ElimInvsTool(..)
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

data ElimInvsTool = ElimInvs { src :: String
                             , out :: String
                             } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

elimInvsMode :: ElimInvsTool
elimInvsMode = ElimInvs { src = srcFlags def
                        , out = outFlags def
                        }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getToolArgs :: IO ElimInvsTool
getToolArgs = parseCmdLnArgs title desc ctors
    where title = "Inverse Elimination Pass"
          desc  = "A command-line interface to inline OpenQASM inv modifiers."
          ctors = [elimInvsMode]
