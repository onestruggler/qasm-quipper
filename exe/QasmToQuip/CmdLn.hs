-- | Command-line parser for the OpenQASM to Quipper translator.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module QasmToQuip.CmdLn
  ( QasmToQuipTool(..)
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

data QasmToQuipTool = QasmToQuip { src :: String
                                 , out :: String
                                 } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

qasmToQuipMode :: QasmToQuipTool
qasmToQuipMode = QasmToQuip { src = srcFlags def
                            , out = outFlags def
                            }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getToolArgs :: IO QasmToQuipTool
getToolArgs = parseCmdLnArgs title desc ctors
    where title = "OpenQASM to Quipper Translator"
          desc  = "A command-line interface to Qasm-Quipper translation tool."
          ctors = [qasmToQuipMode]
