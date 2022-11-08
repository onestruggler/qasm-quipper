-- | Command-line interface to the OpenQASM 3 tools (parser, analyzer, etc.).

{-# LANGUAGE RecordWildCards #-}

import IOUtils (readSrc, withOut)
import Qasm.AST (AstStmt)
import Qasm.Language (Stmt)
import Qasm.Parser (parseQasm)
import QasmCmdLn (QasmTools(..), getToolArgs)
import System.IO (Handle, hPutStrLn)

-------------------------------------------------------------------------------
-- * Utility Types

-- | A function that takes as input the contents of an OpenQASM 3 file, and
-- returns an abstract representation for the program of type a. The second
-- string indicates the input source, and is used for error logging.
type DoTaskFn a = String -> String -> a

-- | A function that takes as input a file handle and the abstraction
-- represerntation of an OpenQASM 3 program of type a. The abstract
-- representation is displayed to the given handle.
type DisplayFn a = Handle -> a -> IO ()

-------------------------------------------------------------------------------
-- * Parser Interface.

parse :: DoTaskFn [Stmt]
parse text file = []

displayAST :: DisplayFn [Stmt]
displayAST hdl ast = hPutStrLn hdl "Parser."

-------------------------------------------------------------------------------
-- * Analyzer Interface.

analyze :: DoTaskFn [AstStmt]
analyze text file = []

displayIR :: DisplayFn [AstStmt]
displayIR hdl ast = hPutStrLn hdl "Analyzer."

-------------------------------------------------------------------------------
-- * Writer Interface.

codegen :: Bool -> DoTaskFn [String]
codegen legacy text file = []

displayQasm :: DisplayFn [String]
displayQasm hdl lines = hPutStrLn hdl "Codegen."

-------------------------------------------------------------------------------
-- * Entry Point.

-- | Takes an input a tool task (doTask), a function to display the results
-- (display), the source text of an OpenQASM 3 program (text), the name of the
-- source file (file), and the handle to an output file (outHdl). Processes the
-- text using doTask, and displays the results to outHdl using display.
runTool :: DoTaskFn a -> DisplayFn a -> String -> String -> Handle -> IO ()
runTool doTask display text file outHdl = display outHdl $ doTask text file

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
processArgs mode@Parser{..}   = setupTool parse displayAST src out
processArgs mode@Analyzer{..} = setupTool analyze displayIR src out
processArgs mode@Writer{..}   = setupTool doTaskFn displayQasm src out
    where doTaskFn = codegen legacy

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args
