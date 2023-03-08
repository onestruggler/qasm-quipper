-- | General-purpose functions to pass along source files to tools.

module LinguaQuantaExe.SetupTools
  ( DoTaskFn
  , DisplayFn
  , setupTool
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import LinguaQuantaExe.IOUtils
  ( readSrc
  , withOut
  )
import System.Exit (die)
import System.IO (Handle)

-------------------------------------------------------------------------------
-- * Tool Wrappers

-- | A function that takes as input the contents of a source 3 file, and
-- returns either an abstract representation for the program of type a, or a
-- parsing error in the form of a string. The first string indicates the input
-- source, and is used for error logging.
type DoTaskFn a = String -> String -> Either a String

-- | A function that takes as input a file handle and the abstraction
-- represerntation of an OpenQASM 3 program of type a. The abstract
-- representation is displayed to the given handle.
type DisplayFn a = Handle -> a -> IO ()

-------------------------------------------------------------------------------
-- * Entry Point

-- | Takes an input a tool task (doTask), a function to display the results
-- (display), the source text of an OpenQASM 3 program (text), the name of the
-- source file (file), and the handle to an output file (outHdl). Processes the
-- text using doTask, and displays the results to outHdl using display.
runTool :: DoTaskFn a -> DisplayFn a -> String -> String -> Handle -> IO ()
runTool doTask display text file outHdl =
    case doTask file text of
        Left rep  -> display outHdl rep
        Right err -> die err

-- | Takes an input a tool task (doTask), a function to display the results
-- (display), an input source (src), and an output destination (dst). Reads all
-- contents from source, acquires a handle for dst, and then dispatches doTask,
-- display, the contents, and the handle to runTool.
setupTool :: DoTaskFn a -> DisplayFn a -> String -> String -> IO ()
setupTool doTask display src out = do
    (text, file) <- readSrc src
    withOut out (runTool doTask display text file)
