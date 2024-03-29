-- | Command-line interface to the Quipper tools (parser, analyzer, etc.).

{-# LANGUAGE RecordWildCards #-}

module Main where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Either (expandLeft)
import LinguaQuanta.Quip.Quipper
  ( GateCirc
  , gatesToAscii
  , parseQuip
  , quipToGates
  )
import LinguaQuantaExe.SetupTools
  ( DoTaskFn
  , DisplayFn
  , setupTool
  )
import QuipTools.CmdLn
  ( QuipTools(..)
  , getToolArgs
  )
import System.IO (hPutStr)
import Text.Pretty.Simple (pHPrint)

-------------------------------------------------------------------------------
-- * Reader Interface.

-- | Composes parsing functions to convert a Quipper ASCII circuit into an
-- abstract gate circuit.
readQuip :: DoTaskFn GateCirc
readQuip file text = Left $ quipToGates $ parseQuip file text

-------------------------------------------------------------------------------
-- * Writer Interface.


-- | Composes parsing functions to convert a Quipper ASCII circuit into an
-- equivalent Quipper ASCII circuit (obtained after performing all processing).
writeQuip :: DoTaskFn String
writeQuip file text = expandLeft (readQuip file text) $
                                 \circ -> Left $ gatesToAscii circ

-------------------------------------------------------------------------------
-- * Entry Point.

-- | Takes as input an instance of QuipTools arguments. The arguments are
-- dispatched to the correct invocation of setupTool.
processArgs :: QuipTools -> IO ()
processArgs mode@Parser{..} = setupTool readQuip pHPrint src out
processArgs mode@Writer{..} = setupTool writeQuip hPutStr src out

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args
