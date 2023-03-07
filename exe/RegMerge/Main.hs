-- | Command-line interface to the OpenQASM 3 register merger pass.

{-# LANGUAGE RecordWildCards #-}

module Main where

-------------------------------------------------------------------------------
-- * Import Section.

import RegMerge.CmdLn
  ( RegMergeTool(..)
  , getToolArgs
  )
import LinguaQuanta.Qasm.AST (AstStmt)
import LinguaQuanta.Qasm.Passes (mergeReg)
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
-- * ElimPows Interface.

doTask :: DoTaskFn [AstStmt]
doTask file input = case parseQasmAST file input of
    Left err  -> Left err
    Right ast -> case mergeReg ast of
        Left ast  -> Right ast
        Right err -> Left $ show err

-------------------------------------------------------------------------------
-- * Entry Point.

processArgs :: RegMergeTool -> IO ()
processArgs mode@RegMerge{..} = setupTool doTask display src out
    where display = printQasmAST False

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args
