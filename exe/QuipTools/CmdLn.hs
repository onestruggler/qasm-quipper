-- | Command-line parser for Quipper analysis tools.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module QuipTools.CmdLn
  ( QuipTools(..)
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

data QuipTools
    = Parser { src :: String
             , out :: String
             }
    | Writer { src :: String 
             , out :: String
             }
    deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

parserMode :: QuipTools
parserMode = addModeAnnotations title desc ctor
    where title = "Parser"
          desc  = "Parses a Quipper program as abstract gates."
          ctor  = Parser { src = srcFlags def
                         , out = outFlags def
                         }

writerMode :: QuipTools
writerMode = addModeAnnotations title desc ctor
    where title = "Writer"
          desc  = "Computes the image of a Quipper program."
          ctor  = Writer { src = srcFlags def
                         , out = outFlags def
                         }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getToolArgs :: IO (QuipTools)
getToolArgs = parseCmdLnArgs title desc ctors
    where title = "Quipper Inspection Tools"
          desc  = "A command-line interface to the Quipper parsing phases."
          ctors = [parserMode, writerMode]
