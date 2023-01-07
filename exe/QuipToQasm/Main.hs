-- | Command-line interface to the Quipper-to-OpenQASM translator.

{-# LANGUAGE RecordWildCards #-}

module Main where

import LinguaQuanta.Qasm.AST (AstStmt)
import LinguaQuanta.Quip.Quipper
  ( parseQuip
  , quipToGates
  )
import LinguaQuanta.QuipToQasm.Translator (translate)
import LinguaQuantaExe.QasmUtils (printQasmAST)
import LinguaQuantaExe.SetupTools
  ( DoTaskFn
  , setupTool
  )
import QuipToQasm.CmdLn
  ( QuipToQasmTool(..)
  , getToolArgs
  )

-------------------------------------------------------------------------------
-- * Translation Interface.

doTask :: DoTaskFn [AstStmt]
doTask file text = Right $ translate $ quipToGates $ parseQuip file text 

-------------------------------------------------------------------------------
-- * Entry Point.

processArgs :: QuipToQasmTool -> IO ()
processArgs mode@QuipToQasm{..} = setupTool doTask display src out
    where display = printQasmAST False

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args
