-- | Command-line interface for Lattice Surgery Compiler pre-processing.

{-# LANGUAGE RecordWildCards #-}

module Main where

-------------------------------------------------------------------------------
-- * Import Section.

import ToLsc.CmdLn
  ( ToLscTool(..)
  , getToolArgs
  )
import LinguaQuanta.Qasm.AST (AstStmt)
import LinguaQuanta.Qasm.Passes (toLsc)
import LinguaQuantaExe.SetupTools
  ( DoTaskFn
  , DisplayFn
  , setupTool
  )
import LinguaQuantaExe.QasmUtils
  ( parseQasmAST
  , printQasmAST
  )

-------------------------------------------------------------------------------
-- * ElimInvs Interface.

doTask :: DoTaskFn [AstStmt]
doTask file input = case parseQasmAST file input of
    Left err  -> Left err
    Right ast -> case toLsc ast of
        Left ast  -> Right ast
        Right err -> Left $ show err

-------------------------------------------------------------------------------
-- * Entry Point.

processArgs :: ToLscTool -> IO ()
processArgs mode@ToLsc{..} = setupTool doTask display src out
    where display = printQasmAST False

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args
