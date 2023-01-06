-- | Command-line interface to the OpenQASM 3 tools (parser, analyzer, etc.).

{-# LANGUAGE RecordWildCards #-}

module Main where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.AST (AstStmt)
import LinguaQuanta.Qasm.Parser (parseQasm)
import LinguaQuanta.Qasm.Printer (printAst)
import LinguaQuantaExe.QasmUtils
  ( parseQasmAST
  , printLines
  )
import LinguaQuantaExe.SetupTools
  ( DoTaskFn
  , setupTool
  )
import QasmTools.CmdLn
  ( QasmTools(..)
  , getToolArgs
  )
import Text.Pretty.Simple (pHPrint)

-------------------------------------------------------------------------------
-- * Writer Interface.

codegen :: Bool -> DoTaskFn [String]
codegen legacy file text =
    case parseQasmAST file text of
        Left err  -> Left err
        Right ast -> Right (printAst legacy ast)

-------------------------------------------------------------------------------
-- * Entry Point.

-- | Takes as input an instance of QasmTools arguments. The arguments are
-- dispatched to the correct invocation of setupTool.
processArgs :: QasmTools -> IO ()
processArgs mode@Parser{..}   = setupTool parseQasm pHPrint src out
processArgs mode@Analyzer{..} = setupTool parseQasmAST pHPrint src out
processArgs mode@Writer{..}   = setupTool doTaskFn printLines src out
    where doTaskFn = codegen legacy

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args
