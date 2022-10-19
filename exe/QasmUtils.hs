-- | Utilities to read OpenQASM 3 programs.

module QasmUtils ( parseQasmFromArgs) where

import Qasm.Language (Stmt(..))
import Qasm.Parser (parseQasm)
import System.Environment (getArgs)

-- | Parses an OpenQASM 3 file, according to the command-line arguments.
-- Namely, if zero arguments are given, then an OpenQASM 3 file is read from
-- stdin. Otherwise, if one argument is given, then it is treated as a filepath
-- from which the OpenQASM 3 file is read. If more then two arguments are
-- given, 
parseQasmFromArgs :: IO (Either String [Stmt])
parseQasmFromArgs = do
    args <- getArgs
    case args of
        []  -> fmap (parseQasm "<stdin>") getContents
        [f] -> fmap (parseQasm f) (readFile f)
        _   -> return (Left "Expect at most 1 argument.")
