{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M05.D24.Solution where

{--
One more bit before we insert rows.

We need to inform the wider audience which articles we've inserted and
updated so they can do stuff. The 'stuff,' in particular, that they are going
to do is the python-y cleaning of these new and updated articles, and they are
doing it in python because that's how they roll.

Incidentally, I was able to download an entire article archive from a REST
endpoint and then upload that article archive with data and metadata to a
PostgreSQL RDS in a couple of minutes.

The Python cleaning interacted solely with the RDS and took 1 hour 10 minutes.

But, there you have it: industrial-strength Python (3.6) is in the mainstream
and Haskell is not because Python is ...

... 50-100x slower.

Anyway, now that I'm done trollin' on the Great Language Debate Flame War ...

For all NEW and UPDATED articles in your triaged set, insert the id into
a new data table: unclean. Later a Python batch job will pick up these ids and
process the associated articles.
--}

import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection
import Store.SQL.Util.Indexed hiding (idx)

import Y2018.M04.D02.Solution (art, idx)
import Y2018.M05.D04.Solution (downloader)
import Y2018.M05.D07.Solution (fetchArticleMetaData)
import Y2018.M05.D08.Solution -- for Triage and WPJATI

{--
Of course, you only get the ids for the new articles after you do the inserts.
I managed to overthink a simple problem! :/

markDirtyArticles :: Connection -> Map Triage [WPJATI] -> IO ()
markDirtyArticles conn =
   storeUncleanArtIds conn . map (idx . art . snd . article)
                           . concat . Map.elems . Map.delete REDUNDANT
--}

-- Above function takes NEW and UPDATED articles and inserts id into unclean
-- To do that, you need to define the below function

storeUncleanArtIds :: Connection -> [Index] -> IO ()
storeUncleanArtIds conn = void . executeMany conn storeUncleanArtIdStmt

-- from the below SQL INSERT statement

storeUncleanArtIdStmt :: Query
storeUncleanArtIdStmt = [sql|INSERT INTO unclean (article_id) VALUES (?)|]

{-- 
Now, with the articles you downloaded from the REST endpoint 
(Y2018.M05.D04.Solution.downloader) and the triage information from the
ArticleMetaData extracted from the database Y2018.M05.D07.fetchArticleMetaData)
store the dirty articles that you (will have) uploaded. How many were there?
--} 

main' :: [String] -> IO ()
main' args = undefined
