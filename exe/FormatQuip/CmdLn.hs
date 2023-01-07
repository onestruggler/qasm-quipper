-- | Command-line parser for Quipper visualization.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module FormatQuip.CmdLn
  ( FormatQuipTool(..)
  , getToolArgs
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuantaExe.CmdLnFlags
  ( QuipVizFormat(..)
  , def
  , outFlags
  , quipVizFlags
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

data FormatQuipTool = FormatQuip { src :: String
                                 , out :: String
                                 , viz :: QuipVizFormat
                                 } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

formatQuipMode :: FormatQuipTool
formatQuipMode = FormatQuip { src = srcFlags def
                            , out = outFlags def
                            , viz = quipVizFlags GateCount
                            }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getToolArgs :: IO FormatQuipTool
getToolArgs = parseCmdLnArgs title desc ctors
    where title = "Quipper Visualizer"
          desc  = "A command-line interface to render Quipper programs."
          ctors = [formatQuipMode]
