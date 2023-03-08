-- | Command-line interface to the Quipper-to-OpenQASM translator.

{-# LANGUAGE RecordWildCards #-}

module Main where

import LinguaQuanta.Either (expandLeft)
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
doTask file text = expandLeft (parseQasmAST file text) $
                              \ast -> Left $ translate ast

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
