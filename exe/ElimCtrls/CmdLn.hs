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
  , disableCCIXFlags
  , elimGateFlags
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

data ElimCtrlsTool = ElimCtrls { src            :: String
                               , out            :: String
                               , elim_toffoli   :: Bool
                               , elim_fredkin   :: Bool
                               , elim_chadamard :: Bool
                               , keep_ccz        :: Bool
                               , disable_ccix   :: Bool
                               } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

elimCtrlsMode :: ElimCtrlsTool
elimCtrlsMode = ElimCtrls { src            = srcFlags def
                          , out            = outFlags def
                          , elim_toffoli   = elimGateFlags "Toffoli-like" def
                          , elim_fredkin   = elimGateFlags "Fredkin" def
                          , elim_chadamard = elimGateFlags "C(H)" def
                          , keep_ccz       = elimGateFlags "CCZ" def
                          , disable_ccix   = disableCCIXFlags def
                          }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getToolArgs :: IO ElimCtrlsTool
getToolArgs = parseCmdLnArgs title desc ctors
    where title = "Control Elimination Transformer"
          desc  = "A command-line interface to inline Quipper controls."
          ctors = [elimCtrlsMode]
