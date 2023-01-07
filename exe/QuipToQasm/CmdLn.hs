-- | Command-line parser for the Quipper to OpenQASM translator.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module QuipToQasm.CmdLn
  ( QuipToQasmTool(..)
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

data QuipToQasmTool = QuipToQasm { src :: String
                                 , out :: String
                                 } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

quipToQasmMode :: QuipToQasmTool
quipToQasmMode = QuipToQasm { src = srcFlags def
                            , out = outFlags def
                            }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getToolArgs :: IO QuipToQasmTool
getToolArgs = parseCmdLnArgs title desc ctors
    where title = "Quipper to OpenQASM Translator"
          desc  = "A command-line interface to Quipper-Qasm translation tool."
          ctors = [quipToQasmMode]
