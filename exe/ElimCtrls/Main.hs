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
  ( ElimCtrlsConf(..)
  , TofRule(..)
  , ZRule(..)
  , applyTransformer
  , elimCtrlsTransformer
  , emptyDruleMap
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

determineZRule :: Bool -> Bool -> ZRule
determineZRule True  _     = DecompCCZ
determineZRule False True  = UseCCZ
determineZRule False False = UseCZ

-------------------------------------------------------------------------------
-- * ElimCtrls Interface.

doTask :: ElimCtrlsConf -> DoTaskFn QuipCirc
doTask conf file input = Left $ applyTransformer elim $ parseQuip file input
    where elim = elimCtrlsTransformer conf

display :: DisplayFn QuipCirc
display hdl = hPutStrLn hdl . gatesToAscii . quipToGates

-------------------------------------------------------------------------------
-- * Entry Point.

processArgs :: ElimCtrlsTool -> IO ()
processArgs mode@ElimCtrls{..} = setupTool taskFn display src out
    where xpolicy = determineTofRule elim_toffoli disable_ccix
          zpolicy = determineZRule elim_toffoli keep_ccz
          taskFn  = doTask $ ElimCtrlsConf { tofRule   = xpolicy
                                           , zRule     = zpolicy
                                           , elimCH    = elim_chadamard
                                           , elimCSwap = elim_fredkin
                                           , druleMap  = emptyDruleMap
                                           }

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args
