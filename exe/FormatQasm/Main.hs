-- | Command-line interface convert between OpenQASM 3 formats.
--
-- Note: Currently, there is only support for OpenQASM 2.0 and OpenQASM 3. In
--particular, there is no visualization support.

{-# LANGUAGE RecordWildCards #-}

module Main where

-------------------------------------------------------------------------------
-- * Import Section.

import FormatQasm.CmdLn
  ( FormatQasmTool(..)
  , getToolArgs
  )
import LinguaQuanta.Qasm.AST (AstStmt)
import LinguaQuanta.Qasm.Passes (elimPow)
import LinguaQuantaExe.SetupTools (setupTool)
import LinguaQuantaExe.QasmUtils
  ( parseQasmAST
  , printQasmAST
  )

-------------------------------------------------------------------------------
-- * Entry Point.

processArgs :: FormatQasmTool -> IO ()
processArgs mode@FormatQasm{..} = setupTool parseQasmAST display src out
    where display = printQasmAST legacy

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args
