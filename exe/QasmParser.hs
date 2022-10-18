-- | Command-line interface to the OpenQASM 3 parser.

module Main where

import Qasm.Parser
import System.Environment

main = do
    -- Parses input according to arguments.
    args <- getArgs
    result <- case args of
        []  -> fmap (parseQasm "<stdin>") getContents
        [f] -> fmap (parseQasm f) (readFile f)
    -- Displays results.
    case result of
        Left str  -> putStrLn str
        Right res -> putStrLn (show res)
