-- | Command-line parser for Lattice Surgery Compiler pre-processing.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module ToLsc.CmdLn
  ( ToLscTool(..)
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

data ToLscTool = ToLsc { src :: String
                       , out :: String
                       } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

toLscMode :: ToLscTool
toLscMode = ToLsc { src = srcFlags def
                  , out = outFlags def
                  }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getToolArgs :: IO ToLscTool
getToolArgs = parseCmdLnArgs title desc ctors
    where title = "Lattice Surgery Compiler Pre-Processing Pass"
          desc  = "A command-line interface to prepare for lattice surgery."
          ctors = [toLscMode]
