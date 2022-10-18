-- | Command-line interface to the OpenQASM 3 parser.

module Main where

import Text.Pretty.Simple (pPrint)
import QasmUtils (parseQasmFromArgs)

main = do
    result <- parseQasmFromArgs
    case result of
        Left str    -> putStrLn str
        Right stmts -> pPrint stmts
