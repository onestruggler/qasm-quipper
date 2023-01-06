-- | Utility functions to process OpenQASM files

module LinguaQuantaExe.QasmUtils
  ( parseQasmAST
  , printLines
  , printQasmAST
  ) where

-------------------------------------------------------------------------------
-- * Import Section..

import LinguaQuanta.Qasm.AST (AstStmt)
import LinguaQuanta.Qasm.Parser (parseQasm)
import LinguaQuanta.Qasm.Passes (toAst)
import LinguaQuanta.Qasm.Printer (printAst)
import LinguaQuantaExe.SetupTools
  ( DoTaskFn
  , DisplayFn
  )
import System.IO (hPutStrLn)

-------------------------------------------------------------------------------
-- * AST Parsing.

-- | Takes as input a filename (file) and its contents (text). If text defines
-- a valid OpenQASM file, then the corresponding AST is returned. Otherwise, a
-- stringified error is returned.
parseQasmAST :: DoTaskFn [AstStmt]
parseQasmAST file text =
    case parseQasm file text of
        Left err  -> Left err
        Right res -> case toAst res of
            Left ast  -> Right ast
            Right err -> Left (show err)

-------------------------------------------------------------------------------
-- * AST Printing.

-- | Takes as input a list of strings. Prints each string (in order) to a new
-- line, and then returns nothing.
printLines :: DisplayFn [String]
printLines _   []           = return ()
printLines hdl (line:lines) = do
    hPutStrLn hdl line
    printLines hdl lines

-- | Takes as input a list of OpenQASM AST statements. Prints each statement
-- (in order) to a new line, and then returns nothing.
printQasmAST :: Bool -> DisplayFn [AstStmt]
printQasmAST legacy hdl = printLines hdl . printAst legacy
