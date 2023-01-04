-- | Command-line interface to the OpenQASM 3 tools (parser, analyzer, etc.).

{-# LANGUAGE RecordWildCards #-}

module Main where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuanta.Qasm.AST (AstStmt)
import LinguaQuanta.Qasm.Parser (parseQasm)
import LinguaQuanta.Qasm.Passes
  ( elimInv
  , elimPow
  , toAst
  )
import LinguaQuanta.Qasm.Printer (printAst)
import QasmTools.CmdLn
  ( QasmTools(..)
  , getToolArgs
  )
import System.IO (hPutStrLn)
import SetupTools
  ( DoTaskFn
  , DisplayFn
  , setupTool
  )
import Text.Pretty.Simple (pHPrint)

-------------------------------------------------------------------------------
-- * Analyzer Interface.

doInlinePow :: [AstStmt] -> Either String [AstStmt]
doInlinePow ast = Right (elimPow ast)

doInlineInv :: Bool -> [AstStmt] -> Either String [AstStmt]
doInlineInv inlinePow ast =
    case elimInv ast of
        Left ast  -> if inlinePow
                     then doInlinePow ast
                     else Right ast
        Right err -> Left (show err)

analyze :: Bool -> Bool -> DoTaskFn [AstStmt]
analyze inlineInv inlinePow file text =
    case parseQasm file text of
        Left err  -> Left err
        Right res -> case toAst res of
            Left ast  -> if inlineInv
                         then doInlineInv inlinePow ast
                         else Right ast
            Right err -> Left (show err)

-------------------------------------------------------------------------------
-- * Writer Interface.

codegen :: Bool -> Bool -> Bool -> DoTaskFn [String]
codegen inlineInv inlinePow legacy file text =
    case analyze inlineInv inlinePow file text of
        Left err  -> Left err
        Right ast -> Right (printAst legacy ast)

displayQasm :: DisplayFn [String]
displayQasm _   []           = return ()
displayQasm hdl (line:lines) = do
    hPutStrLn hdl line
    displayQasm hdl lines

-------------------------------------------------------------------------------
-- * Entry Point.

-- | Takes as input an instance of QasmTools arguments. The arguments are
-- dispatched to the correct invocation of setupTool.
processArgs :: QasmTools -> IO ()
processArgs mode@Parser{..}   = setupTool parseQasm pHPrint src out
processArgs mode@Analyzer{..} = setupTool doTaskFn pHPrint src out
    where doTaskFn = analyze inlineInv inlinePow
processArgs mode@Writer{..}   = setupTool doTaskFn displayQasm src out
    where doTaskFn = codegen inlineInv inlinePow legacy

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args
