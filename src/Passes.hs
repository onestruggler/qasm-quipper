-- | Utilities to implement translation passes.

module Passes
  ( applyPerLinePass
  , applySafePerLinePass
  ) where

-------------------------------------------------------------------------------
-- * Pass Template.

-- | A "per-line pass" is a translation pass that rewrites each statement of an
-- AST independently. A per-line pass is defined by a function f that consumes
-- a line number and program statement (of AST type a), and returns either a
-- list of program statements (each of AST type b) or an error (of type c). The
-- applyPerLinePass function consumes such an f, and returns an analysis pass
-- that applies f to each statement of an AST.
applyPerLinePass :: (Int -> a -> Either [b] c) -> Int -> [a] -> Either [b] c
applyPerLinePass _ _ []           = Left []
applyPerLinePass f n (line:lines) =
    case f n line of
        Left stmt -> case applyPerLinePass f (n + 1) lines of
            Left rest -> Left (stmt ++ rest)
            Right err -> Right err
        Right err -> Right err

-- | An "safe per-line pass" is equivalent to a per-line pass, except that the
-- error case is never encountered.
applySafePerLinePass :: (a -> [b]) -> [a] -> [b]
applySafePerLinePass _ []           = []
applySafePerLinePass f (line:lines) = f line ++ applySafePerLinePass f lines
