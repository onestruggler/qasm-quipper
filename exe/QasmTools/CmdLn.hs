-- | Command-line parser for OpenQasm analysis tools.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module QasmTools.CmdLn
  ( QasmTools(..)
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

data QasmTools
    = Parser { src :: String
             , out :: String
             }
    | Analyzer { src :: String
               , out :: String
               }
    | Writer { src    :: String 
             , out    :: String
             , legacy :: Bool
             }
    deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

parserMode :: QasmTools
parserMode = addModeAnnotations title desc ctor
    where title = "Parser"
          desc  = "Parses an OpenQasm program."
          ctor  = Parser { src = srcFlags def
                         , out = outFlags def
                         }

analyzerMode :: QasmTools
analyzerMode = addModeAnnotations title desc ctor
    where title = "Analyzer"
          desc  = "Computes the internal represntation of an OpenQASM program."
          ctor  = Analyzer { src = srcFlags def
                           , out = outFlags def
                           }

writerMode :: QasmTools
writerMode = addModeAnnotations title desc ctor
    where title = "Writer"
          desc  = "Computes the image of an OpenQASM program."
          ctor  = Writer { src    = srcFlags def
                         , out    = outFlags def
                         , legacy = legacyFlags def
                         }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getToolArgs :: IO QasmTools
getToolArgs = parseCmdLnArgs title desc ctors
    where title = "OpenQASM Inspection Tools"
          desc  = "A command-line interface to the OpenQASM parsing phases."
          ctors = [parserMode, analyzerMode, writerMode]
