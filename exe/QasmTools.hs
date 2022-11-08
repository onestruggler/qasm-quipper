-- | Command-line interface to the OpenQASM 3 tools (parser, analyzer, etc.).

{-# LANGUAGE RecordWildCards #-}

module Main where

import IOUtils (readSrc, withOut)
import QasmCmdLn (QasmTools(..), getToolArgs)
import Qasm.AST (AstStmt)
import Qasm.Parser (parseQasm)
import Qasm.Passes (toAst)
import Qasm.Printer (printAst)
import System.Exit (die)
import System.IO (Handle, hPutStrLn)
import Text.Pretty.Simple (pHPrint)

-------------------------------------------------------------------------------
-- * Utility Types

-- | A function that takes as input the contents of an OpenQASM 3 file, and
-- returns either an abstract representation for the program of type a, or a
-- parsing error in the form of a string. The first string indicates the input
-- source, and is used for error logging.
type DoTaskFn a = String -> String -> Either String a

-- | A function that takes as input a file handle and the abstraction
-- represerntation of an OpenQASM 3 program of type a. The abstract
-- representation is displayed to the given handle.
type DisplayFn a = Handle -> a -> IO ()

-------------------------------------------------------------------------------
-- * Analyzer Interface.

analyze :: DoTaskFn [AstStmt]
analyze file text =
    case parseQasm file text of
        Left err  -> Left err
        Right res -> case toAst 0 res of
            Left ast  -> Right ast
            Right err -> Left (show err)

-------------------------------------------------------------------------------
-- * Writer Interface.

codegen :: Bool -> DoTaskFn [String]
codegen legacy file text =
    case analyze file text of
        Left err  -> Left err
        Right ast -> Right (printAst legacy ast)

displayQasm :: DisplayFn [String]
displayQasm _   []           = return ()
displayQasm hdl (line:lines) = do
    hPutStrLn hdl line
    displayQasm hdl lines

-------------------------------------------------------------------------------
-- * Entry Point.

-- | Takes an input a tool task (doTask), a function to display the results
-- (display), the source text of an OpenQASM 3 program (text), the name of the
-- source file (file), and the handle to an output file (outHdl). Processes the
-- text using doTask, and displays the results to outHdl using display.
runTool :: DoTaskFn a -> DisplayFn a -> String -> String -> Handle -> IO ()
runTool doTask display text file outHdl =
    case doTask file text of
        Left err  -> die err
        Right rep -> display outHdl rep

-- | Takes an input a tool task (doTask), a function to display the results
-- (display), an input source (src), and an output destination (dst). Reads all
-- contents from source, acquires a handle for dst, and then dispatches doTask,
-- display, the contents, and the handle to runTool.
setupTool :: DoTaskFn a -> DisplayFn a -> String -> String -> IO ()
setupTool doTask display src out = do
    (text, file) <- readSrc src
    withOut out (runTool doTask display text file)

-- | Takes an input an instance of QasmTools arguments. The arguments are
-- dispatched to the correct invocation of setupTool.
processArgs :: QasmTools -> IO ()
processArgs mode@Parser{..}   = setupTool parseQasm pHPrint src out
processArgs mode@Analyzer{..} = setupTool analyze pHPrint src out
processArgs mode@Writer{..}   = setupTool doTaskFn displayQasm src out
    where doTaskFn = codegen legacy

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args
