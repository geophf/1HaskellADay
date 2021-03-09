module Y2021.M03.D09.Exercise where

import System.Directory
import System.Environment (getEnv)

{--
Today's #haskell problem: list the contents of a directory and then do
something with each file, like: print the first line of the file.
--}

withDirectoryDo :: FilePath -> (FilePath -> String -> IO ()) -> IO ()
withDirectoryDo dir fileFn = undefined

{-- BONUS -------------------------------------------------------
Add a file-filter into your file-function to work with only Haskell files.
--}
