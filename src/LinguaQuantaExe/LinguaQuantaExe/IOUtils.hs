-- | Utility methods to read source files and output files.

{-# LANGUAGE TupleSections #-}

module LinguaQuantaExe.IOUtils
  ( readSrc
  , redirectStdout
  , setLocalToUtf8
  , withOut
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import GHC.IO.Encoding
  ( setLocaleEncoding
  , utf8
  )

import GHC.IO.Handle
  ( hClose
  , hDuplicate
  , hDuplicateTo
  )
import System.Exit (die)
import System.Directory (doesFileExist)
import System.IO
  ( Handle
  , IOMode(WriteMode)
  , stdout
  , withFile
  )

-------------------------------------------------------------------------------
-- * Basic Input and Output.

-- | Configures IO to use utf8 (this is required for OpenQASM 3 integration).
setLocalToUtf8 :: IO ()
setLocalToUtf8 = setLocaleEncoding utf8

-- | Takes as input the input source. If the source is empty, then (text, file)
-- is returned where text is all contents pending on stdin, and file is set to
-- <stdin>. Otherwise, the input source is read as text, and (text, src) is
-- returned.
readSrc :: String -> IO (String, String)
readSrc ""  = fmap (, "<stdin>") getContents
readSrc src = do
    exists <- doesFileExist src
    if exists
    then fmap (, src) (readFile src)
    else die ("Unable to read source file: " ++ src)

-- Takes as input an output destination and a callback function (cb). If the
-- destination is empty, then cb is called with stdout. Otherwise, cb is called
-- with a handle to the specified file, in write mode.
withOut :: String -> (Handle -> IO ()) -> IO ()
withOut ""  cb = cb stdout
withOut out cb = withFile out WriteMode cb

-------------------------------------------------------------------------------
-- * Stream Manipulation.

-- | Takes as input a file handle (hdl) and a nullary IO function (f). Invokes
-- f with stdout redirected to hdl. After f has terminated, the original handle
-- for stdout is restored.
redirectStdout :: Handle -> (() -> IO ()) -> IO ()
redirectStdout hdl f = do
    if stdout == hdl
    then f ()
    else do
        bk <- hDuplicate stdout
        hDuplicateTo hdl stdout
        f ()
        hDuplicateTo bk stdout
        hClose bk
