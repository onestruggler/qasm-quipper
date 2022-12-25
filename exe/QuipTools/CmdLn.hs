-- | Command-line parser for Quipper analysis tools.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module QuipTools.CmdLn
  ( QuipTools(..)
  , getToolArgs
  ) where

import CmdLnFlags (srcFlags, outFlags)
import System.Console.CmdArgs

-------------------------------------------------------------------------------
-- * Argument Data Type.

data QuipTools
    = Parser { src :: String
             , out :: String
             }
    | Writer { src :: String 
             , out :: String
             }
    deriving (Show,Eq,Data,Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

parserMode :: QuipTools
parserMode = Parser
    { src = srcFlags def
    , out = outFlags def
    } &= details ["Parser:",
                  "Parses a Quipper program as abstract gates."]

writerMode :: QuipTools
writerMode = Writer
    { src       = srcFlags def
    , out       = outFlags def
    } &= details ["Writer:",
                  "Computes the image of a Quipper program."]

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

toolModes :: Mode (CmdArgs QuipTools)
toolModes = cmdArgsMode $ modes [parserMode, writerMode]
    &= summary info
    &= help desc
    &= versionArg [explicit, name "version", name "v", summary vers]
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= helpArg [explicit, name "help", name "h"]
    where summ = "Quipper Transpilation Tools"
          vers = "1.0.0"
          info = summ ++ " version " ++ vers
          desc = "A cmdln interface to the Quipper transpiler pipeline."

-- | Returns all command-line arguments as a QuipTools value.
getToolArgs :: IO (QuipTools)
getToolArgs = cmdArgsRun toolModes
