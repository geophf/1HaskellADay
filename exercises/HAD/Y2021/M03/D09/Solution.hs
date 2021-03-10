module Y2021.M03.D09.Solution where

import Data.List

import System.Directory
import System.Environment (getEnv)

{--
Today's #haskell problem: list the contents of a directory and then do
something with each file, like: print the first line of the file.
--}

withDirectoryDo :: FilePath -> (FilePath -> String -> IO ()) -> IO ()
withDirectoryDo dir fileFn =
   listDirectory dir >>=
   mapM_ (\file -> 
            let ffn = dir ++ ('/':file)
            in  readFile ffn >>= fileFn ffn)

{-- BONUS -------------------------------------------------------
Add a file-filter into your file-function to work with only Haskell files.

>>> let printF filename file =
        if   ".hs" `isSuffixOf` filename
        then putStrLn (filename ++ ":\n") >>
             putStrLn (unlines . take 3 $ lines file)
        else return ()

>>> getEnv "GHC_HOME" >>= flip withDirectoryDo printF . (++ "/Store/SQL/Util")

Store/SQL/Util/Indexed.hs:

module Store.SQL.Util.Indexed where

Store/SQL/Util/Stamping.hs:

module Store.SQL.Util.Stamping where

import Data.Time (LocalTime)

...
--}
