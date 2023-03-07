-- | Command-line parser for power elimination tool.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module RegMerge.CmdLn
  ( RegMergeTool(..)
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

data RegMergeTool = RegMerge { src :: String
                             , out :: String
                             } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

regMergeMode :: RegMergeTool
regMergeMode = RegMerge { src = srcFlags def
                        , out = outFlags def
                        }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getToolArgs :: IO RegMergeTool
getToolArgs = parseCmdLnArgs title desc ctors
    where title = "Register Merger Tool"
          desc  = "A command-line interface to unify OpenQASM registers."
          ctors = [regMergeMode]
