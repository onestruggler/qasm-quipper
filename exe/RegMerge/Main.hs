-- | Command-line interface to the OpenQASM 3 register merger pass.

{-# LANGUAGE RecordWildCards #-}

module Main where

-------------------------------------------------------------------------------
-- * Import Section.

import RegMerge.CmdLn
  ( RegMergeTool(..)
  , getToolArgs
  )
import LinguaQuanta.Either
  ( expandLeft
  , expandRight
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
doTask file input = 
    expandLeft (parseQasmAST file input) $
        \ast -> expandRight (mergeReg ast) $ \err -> Right $ show err

-------------------------------------------------------------------------------
-- * Entry Point.

processArgs :: RegMergeTool -> IO ()
processArgs mode@RegMerge{..} = setupTool doTask display src out
    where display = printQasmAST False

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args
