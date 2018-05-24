{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M05.D24.Exercise where

{--
One more bit before we insert rows.

We need to inform the wider audience which articles we've inserted and
updated so they can do stuff. The 'stuff,' in particular, that they are going
to do is the python-y cleaning of these new and updated articles, and they are
doing it in python because that's how they roll.

For all NEW and UPDATED articles in your triaged set, insert the id into
a new data table: unclean. Later a Python batch job will pick up these ids and
process the associated articles.
--}

import Data.Map (Map)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection
import Store.SQL.Util.Indexed

import Y2018.M05.D04.Exercise (downloader)
import Y2018.M05.D07.Exercise (fetchArticleMetaData)
import Y2018.M05.D08.Exercise -- for Triage and WPJATI

-- this is a bit of misdirection: Why?

markDirtyArticles :: Connection -> Map Triage [WPJATI] -> IO ()
markDirtyArticles triaged = undefined

-- Above function takes NEW and UPDATED articles and inserts id into unclean
-- To do that, you need to define the below function

storeUncleanArtIds :: Connection -> [Integer] -> IO ()
storeUncleanArtIds conn idxn = undefined

-- from the below SQL INSERT statement

storeUncleanArtIdStmt :: Query
storeUncleanArtIdStmt = [sql|INSERT INTO unclean (article_id) VALUES (?)|]

{-- 
Now, with the articles you downloaded from the REST endpoint 
(Y2018.M05.D04.Exercise.downloader) and the triage information from the
ArticleMetaData extracted from the database Y2018.M05.D07.fetchArticleMetaData)
store the dirty articles that you (will have) uploaded. How many were there?
--} 

main' :: [String] -> IO ()
main' args = undefined
