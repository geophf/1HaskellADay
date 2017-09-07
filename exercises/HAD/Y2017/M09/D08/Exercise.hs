module Y2017.M09.D08.Exercise where

import Codec.Compression.GZip
import Data.ByteString.Lazy.Char8 (ByteString)
import Network.HTTP.Conduit
import System.Directory

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
rootURL = "https://github.com/geophf/1HaskellADay/tree/master/exercises/HAD/"

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

bfiles, nfiles :: Directory -> IO [FileName]
bfiles exdir = undefined
nfiles exdir = undefined

-- Now that we've got the file names, load the files, compress them, then save
-- them locally using the same directory structure from where they were fetched

loadCompressStore :: URL -> Directory -> FileName -> IO ()
loadCompressStore url bOrN filename = undefined

{-- BONUS ----------------------------------------------------------------

Instead of storing the newly compressed files on your local file system, store
them instead at some remote location, such as S3 or on a DaaS SQL/NoSQL database

--}

remoteStore :: URL -> Directory -> FileName -> URL -> IO ()
remoteStore url bOrN file remoteURL = undefined
