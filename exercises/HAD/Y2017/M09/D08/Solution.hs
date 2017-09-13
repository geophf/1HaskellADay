module Y2017.M09.D08.Solution where

import qualified Codec.Compression.GZip as GZ
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.HTTP.Conduit
import System.Directory

-- below import available via 1HaskellADay git repository

import Control.Logic.Frege ((<<-))

{--
But, of course, to be an ETL, you're moving data from one source to another
sink, so, let's do that today, but in the opposite direction from what we
did for the D04 exercise.

Today's Haskell problem, we'll take a set of txt files from the 'Web,' compress
them, them move them to a similar directory structure locally.

1. The documents stored remotely are at:
--}

type URL = FilePath

rootURL :: URL
rootURL = "https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/"

type Directory = FilePath

exerciseDir :: Directory
exerciseDir = "Y2017/M09/D08/articles/"

bdir, ndir :: Directory
bdir = "b/"
ndir = "n/"

-- Now, it'd be lolneat if you used the webservice to discover the resources
-- stored in the directories above. 

-- BONUS -----------------------------------------------------------------

-- using the webservice from D05, discover the articles in the directories
-- above.

-- UN-BONUS --------------------------------------------------------------

-- Or you could just use the resources enumerated here from the local file
-- system

type FileName = String

-- the below functions (bfiles, nfiles) reduce to a simpler function

filesAt :: Directory -> Directory -> IO [FileName]
filesAt = getDirectoryContents <<- composeDir

{--
>>> filesAt exerciseDir bdir
[".","..","AP900327-0002.txt","AP900327-0094.txt","AP900327-0242.txt",
 "AP900328-0089.txt","AP900328-0151.txt","AP900329-0129.txt","AP900330-0013.txt",
 "AP900330-0150.txt","AP900401-0056.txt","AP900402-0100.txt"]

Okay, so let's eliminate the meta-directory references, and also concat the
directory from cwd to the files.
--}

composePath :: Directory -> FilePath -> FilePath
composePath x y = x ++ ('/':y)

composeDir :: Directory -> Directory -> Directory
composeDir a b = composePath a b ++ "/"

articlesAt :: Directory -> Directory -> IO [FileName]
articlesAt nbDir exDir = let path = composeDir exDir nbDir in
   fmap (map (path ++) . filter (startsWith '.')) $ filesAt exDir nbDir

startsWith :: Char -> String -> Bool
startsWith c (h:_) = c /= h

-- so with articlesAt we can write our b/n files functions:

bfiles, nfiles :: Directory -> IO [FileName]
bfiles = articlesAt bdir
nfiles = articlesAt ndir

{--
>>> bfiles exerciseDir 
["Y2017/M09/D08/articles//b//AP900327-0002.txt",
 "Y2017/M09/D08/articles//b//AP900327-0094.txt",
 "Y2017/M09/D08/articles//b//AP900327-0242.txt",
 "Y2017/M09/D08/articles//b//AP900328-0089.txt",
 "Y2017/M09/D08/articles//b//AP900328-0151.txt",
 "Y2017/M09/D08/articles//b//AP900329-0129.txt",
 "Y2017/M09/D08/articles//b//AP900330-0013.txt",
 "Y2017/M09/D08/articles//b//AP900330-0150.txt",
 "Y2017/M09/D08/articles//b//AP900401-0056.txt",
 "Y2017/M09/D08/articles//b//AP900402-0100.txt"]

YYYUUUUSSSS!!!
--}

-- Now that we've got the file names, load the files, compress them, then save
-- them locally using the same directory structure from where they were fetched

loadCompressStore :: URL -> Directory -> Directory -> FileName -> IO ()
loadCompressStore url archiveDir bOrN filename =
   let subArch = composeDir archiveDir bOrN
       archDir = composeDir archiveDir (composeDir "zipped/" bOrN) in
   createDirectoryIfMissing True archDir                                >>
   simpleHttp (composePath (composeDir url subArch) filename)           >>=
   BL.writeFile (composePath archDir (filename ++ ".gz")) . GZ.compress

{--
>>> fmap (filter (startsWith '.')) (filesAt exerciseDir bdir) >>=
    mapM_ (loadCompressStore rootURL exerciseDir bdir)

(do an ls on the archived directory and...)

$ ls -lR Y2017/M09/D08/articles/

... and ...
Y2017/M09/D08/articles//zipped/b:
total 80
-rw-r--r--  1 geophf  staff   703 Sep 12 20:23 AP900327-0002.txt.gz
-rw-r--r--  1 geophf  staff   607 Sep 12 20:23 AP900327-0094.txt.gz
-rw-r--r--  1 geophf  staff  1562 Sep 12 20:23 AP900327-0242.txt.gz
-rw-r--r--  1 geophf  staff  1003 Sep 12 20:23 AP900328-0089.txt.gz
-rw-r--r--  1 geophf  staff   848 Sep 12 20:23 AP900328-0151.txt.gz
-rw-r--r--  1 geophf  staff  1473 Sep 12 20:23 AP900329-0129.txt.gz
-rw-r--r--  1 geophf  staff   656 Sep 12 20:23 AP900330-0013.txt.gz
-rw-r--r--  1 geophf  staff  1789 Sep 12 20:23 AP900330-0150.txt.gz
-rw-r--r--  1 geophf  staff  1288 Sep 12 20:23 AP900401-0056.txt.gz
-rw-r--r--  1 geophf  staff  2110 Sep 12 20:23 AP900402-0100.txt.gz
--}

{-- BONUS ----------------------------------------------------------------

Instead of storing the newly compressed files on your local file system, store
them instead at some remote location, such as S3 or on a DaaS SQL/NoSQL database

--}

remoteStore :: URL -> Directory -> FileName -> URL -> IO ()
remoteStore url bOrN file remoteURL = undefined

-- We will do remoteStore as a separate exercise.
