-- | Tools to summary OpenQASM file headers.

module LinguaQuanta.Qasm.Header
  ( LibErr(..)
  , QasmHeader
  , addLib
  , isLegacy
  , toQasmHeader
  , usingBkp
  , usingQfn
  , usingQpr
  , usingStd
  ) where

-------------------------------------------------------------------------------
-- * Header Data Structures.

-- | Summarizes the contents of an OpenQASM file header. Note that:
-- 1. std refers to the OpenQASM standard library;
-- 2. bkp refers to the LinguaQuanta of the OpenQASM 3 standard library;
-- 3. qpr refers to the LinguaQunata Quipper gate library;
-- 4. qfn refers to the LinguaQuanta Quipper function library.
data QasmHeader = QasmHeader { isLegacy :: Bool
                             , usingStd :: Bool
                             , usingBkp :: Bool
                             , usingQpr :: Bool
                             , usingQfn :: Bool }
                             deriving (Show, Eq)

-------------------------------------------------------------------------------
-- * Header Analysis Methods.

-- | An error summarizing why a library is invalid (with respect to an OpenQASM
-- file header).
data LibErr = NonLegacyLib String
            | LegacyLib String
            | DoubleImport String 
            | MissingDep String String
            | UnknownLib String
            deriving (Show, Eq)

-- | Returns the standard library name for a given version of OpenQASM.
stdLibName :: Bool -> String
stdLibName True  = "qelib1.inc"
stdLibName False = "stdgates.inc"

-- | Takes as input an OpenQASM version string. If the version string is valid,
-- then a default header is returned for the given version (i.e., a header
-- without any library imports nor pragmas). Otherwise, nothing is returned.
toQasmHeader :: String -> Maybe QasmHeader
toQasmHeader "2.0" = Just $ QasmHeader True False False False False
toQasmHeader "3"   = Just $ QasmHeader False False False False False
toQasmHeader _     = Nothing

-- | Takes as input the name of a library, and a partial QasmHeader. If the
-- library is compatible with the header, then a new header is returned which
-- cobines the previous summary with the new library. Otherwise, an error is
-- returned describing why the library is not valid.
addLib :: String -> QasmHeader -> Either QasmHeader LibErr
addLib name@"stdgates.inc" (QasmHeader legacy std bkp qpr qfn)
    | legacy    = Right $ NonLegacyLib name
    | std       = Right $ DoubleImport name
    | otherwise = Left  $ QasmHeader legacy True bkp qpr qfn
addLib name@"qelib1.inc" (QasmHeader legacy std bkp qpr qfn)
    | not legacy = Right $ LegacyLib name
    | std        = Right $ DoubleImport name
    | otherwise  = Left  $ QasmHeader legacy True bkp qpr qfn
addLib name@"bkpgates.inc" (QasmHeader legacy std bkp qpr qfn)
    | not legacy = Right $ LegacyLib name
    | not std    = Right $ MissingDep name $ stdLibName legacy
    | bkp        = Right $ DoubleImport name
    | otherwise  = Left  $ QasmHeader legacy std True qpr qfn
addLib name@"quipgates.inc" (QasmHeader legacy std bkp qpr qfn)
    | not std   = Right $ MissingDep name $ stdLibName legacy
    | qpr       = Right $ DoubleImport name
    | otherwise = Left  $ QasmHeader legacy std bkp True qfn
addLib name@"quipfuncs.inc" (QasmHeader legacy std bkp qpr qfn)
    | legacy    = Right $ NonLegacyLib name
    | not std   = Right $ MissingDep name $ stdLibName legacy
    | qfn       = Right $ DoubleImport name
    | otherwise = Left  $ QasmHeader legacy std bkp qpr True
addLib name _ = Right $ UnknownLib name
