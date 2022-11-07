-- | Command-line parser for OpenQasm analysis tools.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

import System.Console.CmdArgs

-------------------------------------------------------------------------------
-- * Argument Data Type.

data QasmTools
    = Reader { src :: String
             , out :: String
             }
    | Analyzer { src :: String
               , out :: String
               }
    | Writer { src    :: String 
             , out    :: String
             , legacy :: Bool
             }
    deriving (Show,Eq,Data,Typeable)

-------------------------------------------------------------------------------
-- * Flag Generators.

-- | Returns the flags for the --src argument. The default value is taken as an
-- argument, since flags are impure.
srcFlags :: String -> String
srcFlags x = x &= help "Input source (defaults to stdin)."
               &= typFile

-- | Returns the flags for the --out argument. The default value is taken as an
-- argument, since flags are impure.
outFlags :: String -> String
outFlags x = x &= help "Output destination (defaults to stdout)."
               &= typFile

-- | Returns the flags for the --legacy argument. The default value is taken as
-- an argument, since flags are impure.
legacyFlags :: Bool -> Bool
legacyFlags x = x &= help "Generate OpenQASM 2.0 output"

-------------------------------------------------------------------------------
-- * Program Modes.

readerMode :: QasmTools
readerMode = Reader
    { src = srcFlags def
    , out = outFlags def
    } &= details ["Reader:",
                  "Parses an OpenQasm program."]

analyzerMode :: QasmTools
analyzerMode = Analyzer
    { src = srcFlags def
    , out = outFlags def
    } &= details ["Analyzer:",
                  "Computes the internal represntation of an OpenQASM program."]

writerMode :: QasmTools
writerMode = Writer
    { src    = srcFlags def
    , out    = outFlags def
    , legacy = legacyFlags def
    } &= details ["Writer:",
                  "Computes the image of an OpenQASM program."]

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

toolModes :: Mode (CmdArgs QasmTools)
toolModes = cmdArgsMode $ modes [readerMode, analyzerMode, writerMode]
    &= summary info
    &= help desc
    &= versionArg [explicit, name "version", name "v", summary vers]
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= helpArg [explicit, name "help", name "h"]
    where summ = "OpenQASM Transpilation Tools"
          vers = "1.0.0"
          info = summ ++ " version " ++ vers
          desc = "Provides a cmdln interface to the OpenQASM transpiler pipeline."

-- | Returns all command-line arguments as a QasmTools value.
getToolArgs :: IO (QasmTools)
getToolArgs = cmdArgsRun toolModes

main = print =<< getToolArgs
