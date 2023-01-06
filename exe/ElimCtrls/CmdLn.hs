-- | Command-line parser for control elimination tool.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module ElimCtrls.CmdLn
  ( ElimCtrlsTool(..)
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

data ElimCtrlsTool = ElimCtrls { src :: String
                               , out :: String
                               } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

elimCtrlsMode :: ElimCtrlsTool
elimCtrlsMode = ElimCtrls { src = srcFlags def
                          , out = outFlags def
                          }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getToolArgs :: IO ElimCtrlsTool
getToolArgs = parseCmdLnArgs title desc ctors
    where title = "Control Elimination Transformer"
          desc  = "A command-line interface to inline Quipper controls."
          ctors = [elimCtrlsMode]
