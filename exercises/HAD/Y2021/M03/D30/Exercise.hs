module Y2021.M03.D30.Exercise where

import Data.List (isSuffixOf)

import System.Directory (listDirectory)

{--
Okay, today, we're going to list (presumably to operate on) certain kinds of
files in a directory. To do that we need to build a filter-function.
--}

filterer :: [String] -> FilePath -> Bool
filterer suffixes file = undefined

-- filterer returns True if file ends with one of the suffixes

-- with filterer, list the contents of this directory of files that end with
-- ".txt" or ".dat" (remember those good old days?) or ".hs"

sourceFiles :: FilePath -> IO [FilePath]
sourceFiles dir = undefined
