-- | Command-line interface to the OpenQASM 3 static analyzer.

import Qasm.Parser
import Qasm.Passes
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
        Right res -> case toAst 0 res of
            Left ast  -> putStrLn (show ast)
            Right err -> putStrLn (show err)
