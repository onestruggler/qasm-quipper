-- | Command-line interface to the OpenQASM 3 static analyzer.

import Text.Pretty.Simple (pPrint)
import QasmUtils (parseQasmFromArgs)
import Qasm.Passes

main = do
    result <- parseQasmFromArgs
    case result of
        Left str  -> putStrLn str
        Right res -> case toAst 0 res of
            Left ast  -> pPrint ast
            Right err -> putStrLn (show err)
