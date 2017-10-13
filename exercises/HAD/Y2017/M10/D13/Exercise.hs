module Y2017.M10.D13.Exercise where

{--
Today, for something COMPLETELY different, ... yet UTTERLY the same ...

We're going to build and then deploy an application.

Take the ETL process you've built over the ... is it a month now? IT'S A MONTH!
... last month and create an application. Run it on the 'command line' or the
shell or as a web service. Use the below referenced filepaths as compressed
archived and load these data sets up to your PostgreSQL data store.

LOOK AT YOU! PRODUCTION READY!

But your application needs a name! What shall you call your app?

Well, obviously: Y2017.M10.D13.Solution, of course!

Let's do this.
--}

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time
import Data.Time.Clock
import Database.PostgreSQL.Simple
import System.Environment

-- below imports available via 1HaskellADay git repository

import Data.MemoizingTable (MemoizingTable)
import qualified Data.MemoizingTable as Mem
import Store.SQL.Connection
import Store.SQL.Util.Indexed

import Y2017.M09.D25.Exercise     -- Article type
import Y2017.M10.D03.Exercise     -- subject memoization

main :: IO ()
main = undefined

-- main takes a filepath argument, reads in that compressed archive and stores
-- the reified result into the PostgreSQL database.

{--
Hints.

* read command-line arguments using functions from System.Environment.
* the timed ETL process is in Y2017.M10.D03.Exercise. Can you just grab that
  and go? No:
  * The previous ETL process did not scan the subject table before
    processing articles. We need to initialize the MemoizingTable from the
    database (which is already declared in Y2017.M10.D03.Exercise). So we must
    roll fetching the pre-existing subjects into the ETL definition.
--}

mt2IxSubjects :: MemoizingTable Integer Subject -> [IxValue String]
mt2IxSubjects mt = undefined

compressedArchives :: [FilePath]
compressedArchives =
   map ((nyt ++) . (++ ".txt.gz") . show) [ONE, TWO]
      where nyt = "Y2017/M10/D13/NYTOnline_08-29-17_09-05-17_pt"

-- So, now that we've got the data, what do we do with it? ANALYZE IT!
-- Next week we'll be doing some data analytics.
