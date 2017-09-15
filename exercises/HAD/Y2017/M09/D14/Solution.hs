{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M09.D14.Solution where

import Control.Monad (void)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

import Network.HTTP.Conduit

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection (connectInfo)

{--
Today, we're going to upload the articles, their dates, and the article text
onto a postgre SQL database. You can do this on your own computer, if you have
that DBMS running, or you can upload to a DaaS (there are several, e.g.:
elephantSQL.com).

The first thing we need to do is to read in the articles and compress them. 
Well, we've done that:
--}

import Y2017.M09.D08.Solution (rootURL, filesAt, startsWith, URL, Directory, FileName, composeDir, composePath)
import Y2017.M09.D13.Solution (fileNameToDay, articlesDir)

{--
But instead of writing the compressed files out to the file-system, we'll
put them into a structure and (eventually) write those structures out to
the database.

Now, if we were using Database.Persist.PostgreSQL we'd do something like this:

share [mkPersist sqlSettings] [persistLowerCase|
   Articles
      Id       sql=id
      articleId   String
      publishedDt Day
      rawText     String
|]

But, we're using the Database.Postgresql.Simple-approach, so let's go!
--}

data Article =
   Art { artId :: String, published :: UTCTime, summary, text :: ByteString }

instance Show Article where
   show (Art a b c _) = unwords ["Art", a,show b, BL.unpack c]
instance ToRow Article where
   toRow (Art a b c d) = [toField a, toField b, toField c, toField d]

loadCompress :: URL -> Directory -> FileName -> IO Article

-- doing some 'creative ripping' from Y2017.M09.D08.loadCompressStore ...

loadCompress url archive filename =
   simpleHttp (composePath (composeDir url archive) filename) >>= \artTxt ->
   return (Art filename
               (fileNameToDay filename)
               (summary artTxt)
               artTxt)

   -- (GZ.compress artTxt)) -- I don't need to compress (see comments on TOAST)

      where summary = BL.unwords . take 10 . BL.words

{--
>>> loadCompress rootURL articlesDir (artId sampleInsert2)
Art AP900327-0002.txt Here are the prime-time TV ratings as compiled by the 
    1990-03-27 00:02:00 UTC
--}

-- Now, with that set of articles, we'll upload them to the database to
-- a table with the structure of Article:

insertArticles :: Connection -> [Article] -> IO ()
insertArticles conn = void . executeMany conn insertStmt

-- The insert statement looks like this:

insertStmt :: Query
insertStmt =
   [sql|INSERT INTO article
        (article_id, publication_dt, summary, raw_text)
        VALUES (?,?,?,?)|]

sampleInsert1, sampleInsert2 :: Article
sampleInsert1 =
   let art = "AP900821-0227.txt" in
   Art art
       (fileNameToDay art) "sum-sum-sum" "blah, blah"
sampleInsert2 =
   let artid = "AP900327-0002.txt" in
   Art artid
       (fileNameToDay artid)
       "Here are the primetime TV ratings as compiled by the A.C. Nielsen"
       "blah, di, blah"

{--
>>> connectInfo
ConnectInfo { connectHost = "..." ...}
>>> conn <- connect it
>>> execute conn insertStmt (asTup sampleInsert)
1

Whoa! I got a '1' response. Go me!

Now let's tie it all together:
--}

loadAllArts :: URL -> Directory -> IO [Article]
loadAllArts url dir = fmap (filter (startsWith '.')) (filesAt dir "") >>=
                      mapM (loadCompress url dir)

{--
>>> fmap (take 3) (loadAllArts rootURL articlesDir)
[Art AP900327-0002.txt Here are the prime-time TV ratings as compiled by the 1990-03-27 00:02:00 UTC,
 Art AP900327-0094.txt The Census Bureau is increasing the number of telephone help 1990-03-27 01:34:00 UTC,
 Art AP900327-0242.txt Rep. Dan Rostenkowski, the futures industry's most powerful ally in 1990-03-27 02:42:00 UTC]

With that, we create an ETL to load the Associated Press articles into our
SQL store.
--}

etlAP :: Connection -> URL -> Directory -> IO ()
etlAP conn url dir = loadAllArts url dir >>= insertArticles conn

{--
>>> connectInfo 
ConnectInfo {connectHost = "..." ...}
>>> conn <- connect it
>>> etlAP conn rootURL articlesDir 
*** Exception: SqlError {sqlState = "22021", sqlExecStatus = FatalError, sqlErrorMsg = "invalid byte sequence for encoding \"UTF8\": 0x8b", sqlErrorDetail = "", sqlErrorHint = ""}

... whoopsies! ... is it the compression? [removes the GZ.compress]

... [redo without the gzip]

... yup, that worked. I'll look into storing compressed files later.

... and I looked. PostGre does 'automagical' compress with TOAST ("the greatest
thing since sliced bread!"), so I don't have to use the compression stuff in
Haskell, as the database I selected compresses and decompresses transparently.

lolneat.
--}
