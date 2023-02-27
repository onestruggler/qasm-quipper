-- | Command-line parser for inlining tool.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module ElimFuns.CmdLn
  ( ElimFunsTool(..)
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

data ElimFunsTool = ElimFuns { src :: String
                             , out :: String
                             } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

elimFunsMode :: ElimFunsTool
elimFunsMode = ElimFuns { src = srcFlags def
                        , out = outFlags def
                        }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getToolArgs :: IO ElimFunsTool
getToolArgs = parseCmdLnArgs title desc ctors
    where title = "Function Elimination Pass"
          line1 = "A command-line interface to eliminate function calls."
          line2 = "Only functions built into OpenQASM 2.0 are retained."
          desc  = line1 ++ " " ++ line2
          ctors = [elimFunsMode]
