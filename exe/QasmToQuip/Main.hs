-- | Command-line interface to the Quipper-to-OpenQASM translator.

{-# LANGUAGE RecordWildCards #-}

module Main where

import LinguaQuanta.QasmToQuip.Translator (translate)
import LinguaQuanta.Quip.Quipper
  ( GateCirc
  , gatesToAscii
  )
import LinguaQuantaExe.QasmUtils (parseQasmAST)
import LinguaQuantaExe.SetupTools
  ( DoTaskFn
  , DisplayFn
  , setupTool
  )
import QasmToQuip.CmdLn
  ( QasmToQuipTool(..)
  , getToolArgs
  )
import System.IO (hPutStr)

-------------------------------------------------------------------------------
-- * Translation Interface.

doTask :: DoTaskFn GateCirc
doTask file text = case parseQasmAST file text of
    Left err  -> Left err
    Right ast -> Right $ translate ast

display :: DisplayFn GateCirc
display hdl circ = hPutStr hdl $ gatesToAscii circ

-------------------------------------------------------------------------------
-- * Entry Point.

processArgs :: QasmToQuipTool -> IO ()
processArgs mode@QasmToQuip{..} = setupTool doTask display src out

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args
