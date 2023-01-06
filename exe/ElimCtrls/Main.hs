-- | Command-line interface to the Quipper control elimination transformation.

{-# LANGUAGE RecordWildCards #-}

module Main where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Quip.Quipper
  ( QuipCirc
  , gatesToAscii
  , parseQuip
  , quipToGates
  )
import LinguaQuanta.Quip.Transformers
  ( applyTransformer
  , elimCtrlsTransformer
  )
import LinguaQuantaExe.SetupTools
  ( DoTaskFn
  , DisplayFn
  , setupTool
  )
import ElimCtrls.CmdLn
  ( ElimCtrlsTool(..)
  , getToolArgs
  )
import System.IO (hPutStrLn)

-------------------------------------------------------------------------------
-- * ElimCtrls Interface.

doTask :: DoTaskFn QuipCirc
doTask file input = Right $ applyTransformer elimCtrlsTransformer
                          $ parseQuip file input

display :: DisplayFn QuipCirc
display hdl = hPutStrLn hdl . gatesToAscii . quipToGates

-------------------------------------------------------------------------------
-- * Entry Point.

processArgs :: ElimCtrlsTool -> IO ()
processArgs mode@ElimCtrls{..} = setupTool doTask display src out

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args
