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

doTask :: Bool -> Bool -> Bool -> DoTaskFn QuipCirc
doTask elimTof elimCH elimCSwap file input = Left $ applyTransformer elim
                                                  $ parseQuip file input
    where elim = elimCtrlsTransformer elimTof elimCH elimCSwap

display :: DisplayFn QuipCirc
display hdl = hPutStrLn hdl . gatesToAscii . quipToGates

-------------------------------------------------------------------------------
-- * Entry Point.

processArgs :: ElimCtrlsTool -> IO ()
processArgs mode@ElimCtrls{..} = setupTool taskFn display src out
    where taskFn = doTask elim_toffoli elim_chadamard elim_fredkin

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args
