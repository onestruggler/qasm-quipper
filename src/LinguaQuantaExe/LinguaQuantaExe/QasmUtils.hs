-- | Utility functions to process OpenQASM files

module LinguaQuantaExe.QasmUtils
  ( parseQasmAST
  , printLines
  , printQasmAST
  ) where

-------------------------------------------------------------------------------
-- * Import Section..

import LinguaQuanta.Qasm.AST (AstStmt)
import LinguaQuanta.Qasm.Header
  ( QasmHeader
  , addLib
  , toQasmHeader
  )
import LinguaQuanta.Qasm.Language
  ( Program(..)
  , QasmInclude(..)
  )
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

-- | Takes as input a partial OpenQASM header, and a list of includes. Attempts
-- to add each library to the header. If the fold is successful, then the new
-- header is returned. Otherwise, an error is returned describing the first
-- failure (from head to tail).
analyzeIncludes :: QasmHeader -> [QasmInclude] -> Either String QasmHeader
analyzeIncludes header []                       = Right header
analyzeIncludes header (QasmInclude incl:incls) =
    case addLib name header of
        Left header' -> analyzeIncludes header' incls
        Right err    -> Left $ show err
    where name = tail $ init incl

-- | Takes as input the version of an OpenQASM file, and its list of includes.
-- If the includes are valid relative to the version string, then a header
-- summary is returned. Otherwise, an error is returned describing the first
-- failure (from head to tail).
analyzerHeader :: String -> [QasmInclude] -> Either String QasmHeader
analyzerHeader vers incls =
    case toQasmHeader vers of
        Just header -> analyzeIncludes header incls
        Nothing     -> Left $ "Unknown OpenQASM version: " ++ vers

-- | Takes as input a filename (file) and its contents (text). If text defines
-- a valid OpenQASM file, then the corresponding AST is returned. Otherwise, a
-- stringified error is returned.
parseQasmAST :: DoTaskFn [AstStmt]
parseQasmAST file text =
    case parseQasm file text of
        Left err                         -> Left err
        Right (Program vers incls stmts) -> case analyzerHeader vers incls of
            Left err     -> Left err
            Right header -> case toAst stmts of
                Left ast  -> Right ast
                Right err -> Left $ show err

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
