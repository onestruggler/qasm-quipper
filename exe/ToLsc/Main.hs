-- | Command-line interface for Lattice Surgery Compiler pre-processing.

{-# LANGUAGE RecordWildCards #-}

module Main where

-------------------------------------------------------------------------------
-- * Import Section.

import ToLsc.CmdLn
  ( ToLscTool(..)
  , getToolArgs
  )
import LinguaQuanta.Either
  ( expandLeft
  , expandRight
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
doTask file input = 
    expandLeft (parseQasmAST file input) $
        \ast -> expandRight (toLsc ast) $ \err -> Right $ show err

-------------------------------------------------------------------------------
-- * Entry Point.

processArgs :: ToLscTool -> IO ()
processArgs mode@ToLsc{..} = setupTool doTask display src out
    where display = printQasmAST False

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args
