-- | Command-line interface to the OpenQASM 3 static analyzer.

import Text.Pretty.Simple (pPrint)
import QasmUtils (parseQasmFromArgs)
import Qasm.Passes (toAst)
import Qasm.Printer (printAst)

printLines :: [String] -> IO ()
printLines []           = return ()
printLines (line:lines) = do
    putStrLn line
    printLines lines

main = do
    result <- parseQasmFromArgs
    case result of
        Left str  -> putStrLn str
        Right res -> case toAst 0 res of
            Left ast  -> printLines (printAst False ast)
            Right err -> putStrLn (show err)
