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
  ( TofRule(..)
  , applyTransformer
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
-- * Argument Post-Processing.

determineTofRule :: Bool -> Bool -> TofRule
determineTofRule True  _     = ElimTof
determineTofRule False True  = UseTof
determineTofRule False False = UseCCIX

-------------------------------------------------------------------------------
-- * ElimCtrls Interface.

doTask :: TofRule -> Bool -> Bool -> DoTaskFn QuipCirc
doTask tofRule elimCH elimCSwap file input = Left $ applyTransformer elim
                                                  $ parseQuip file input
    where elim = elimCtrlsTransformer tofRule elimCH elimCSwap

display :: DisplayFn QuipCirc
display hdl = hPutStrLn hdl . gatesToAscii . quipToGates

-------------------------------------------------------------------------------
-- * Entry Point.

processArgs :: ElimCtrlsTool -> IO ()
processArgs mode@ElimCtrls{..} = setupTool taskFn display src out
    where tofRule = determineTofRule elim_toffoli disable_ccix
          taskFn  = doTask tofRule elim_chadamard elim_fredkin

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args
