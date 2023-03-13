-- | Command-line interface to the OpenQASM 3 pow modifier elimination pass.

{-# LANGUAGE RecordWildCards #-}

module Main where

-------------------------------------------------------------------------------
-- * Import Section.

import ElimPows.CmdLn
  ( ElimPowsTool(..)
  , getToolArgs
  )
import LinguaQuanta.Either (expandLeft)
import LinguaQuanta.Qasm.AST (AstStmt)
import LinguaQuanta.Qasm.Passes (elimPow)
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
doTask file input = expandLeft (parseQasmAST file input) $
                               \ast -> Left $ elimPow ast

-------------------------------------------------------------------------------
-- * Entry Point.

processArgs :: ElimPowsTool -> IO ()
processArgs mode@ElimPows{..} = setupTool doTask display src out
    where display = printQasmAST False

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args
