-- | Utility methods to read source files and output files.

{-# LANGUAGE TupleSections #-}

module IOUtils
  ( readSrc
  , withOut
  ) where

import System.Exit (die)
import System.Directory (doesFileExist)
import System.IO (Handle, IOMode(WriteMode), withFile, stdout)

-------------------------------------------------------------------------------
-- * Helper Methods.

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
