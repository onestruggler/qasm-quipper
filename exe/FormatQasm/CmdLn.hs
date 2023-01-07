-- | Command-line parser for power elimination tool.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module FormatQasm.CmdLn
  ( FormatQasmTool(..)
  , getToolArgs
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuantaExe.CmdLnFlags
  ( def
  , legacyFlags
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

data FormatQasmTool = FormatQasm { src    :: String
                                 , out    :: String
                                 , legacy :: Bool
                                 } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

formatQasmMode :: FormatQasmTool
formatQasmMode = FormatQasm { src    = srcFlags def
                            , out    = outFlags def
                            , legacy = legacyFlags def
                            }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getToolArgs :: IO FormatQasmTool
getToolArgs = parseCmdLnArgs title desc ctors
    where title = "OpenQASM Output Formatter"
          desc  = "A command-line interface to convert to OpenQASM formats."
          ctors = [formatQasmMode]
