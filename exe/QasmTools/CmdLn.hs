-- | Command-line parser for OpenQasm analysis tools.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module QasmTools.CmdLn
  ( QasmTools(..)
  , getToolArgs
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import CmdLnFlags
  ( inlineInvFlags
  , inlinePowFlags
  , legacyFlags
  , outFlags
  , srcFlags
  )
import System.Console.CmdArgs

-------------------------------------------------------------------------------
-- * Argument Data Type.

data QasmTools
    = Parser { src :: String
             , out :: String
             }
    | Analyzer { src       :: String
               , out       :: String
               , inlinePow :: Bool
               , inlineInv :: Bool
               }
    | Writer { src       :: String 
             , out       :: String
             , inlinePow :: Bool
             , inlineInv :: Bool
             , legacy    :: Bool
             }
    deriving (Show,Eq,Data,Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

parserMode :: QasmTools
parserMode = Parser
    { src = srcFlags def
    , out = outFlags def
    } &= details ["Parser:",
                  "Parses an OpenQasm program."]

analyzerMode :: QasmTools
analyzerMode = Analyzer
    { src = srcFlags def
    , out = outFlags def
    , inlinePow = inlinePowFlags def
    , inlineInv = inlineInvFlags def
    } &= details ["Analyzer:",
                  "Computes the internal represntation of an OpenQASM program."]

writerMode :: QasmTools
writerMode = Writer
    { src       = srcFlags def
    , out       = outFlags def
    , inlinePow = inlinePowFlags def
    , inlineInv = inlineInvFlags def
    , legacy    = legacyFlags def
    } &= details ["Writer:",
                  "Computes the image of an OpenQASM program."]

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

toolModes :: Mode (CmdArgs QasmTools)
toolModes = cmdArgsMode $ modes [parserMode, analyzerMode, writerMode]
    &= summary info
    &= help desc
    &= versionArg [explicit, name "version", name "v", summary vers]
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= helpArg [explicit, name "help", name "h"]
    where summ = "OpenQASM Transpilation Tools"
          vers = "1.0.0"
          info = summ ++ " version " ++ vers
          desc = "A cmdln interface to the OpenQASM transpiler pipeline."

-- | Returns all command-line arguments as a QasmTools value.
getToolArgs :: IO (QasmTools)
getToolArgs = cmdArgsRun toolModes
