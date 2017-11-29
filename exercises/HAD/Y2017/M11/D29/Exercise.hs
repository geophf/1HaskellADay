{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M11.D29.Exercise where

{--
So, now that we've added and deleted recommended articles to our recommended
articles list, we need to have our grande poobah user select the ones to be
published, save those off somewhere, and then, well, publish them. That is to
say: return, at a later time, the list of articles to be published alongside
the source article.

Let's do that today.
--}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection

import Y2017.M11.D21.Exercise  -- for Brief-type
import Y2017.M11.D24.Exercise  -- for loading Briefs from the database

-- (low-key: anybody else say 'brieves'? 'brèves'?) (the French in me)

-- So we need a publish type that includes the source article id, the 
-- recommendation id and a ranking: 1,2,3,4 ...

data Publish = Pub { srcIdx, recIdx, rank :: Integer }
   deriving (Eq, Show)

instance ToRow Publish where
   toRow pub = undefined

instance FromRow Publish where
   fromRow = undefined

-- today, we're just going to upload this publishing info; tomorrow we'll look
-- at extracting article information for articles to be published

insertPubsStmt :: Query
insertPubsStmt =
   [sql|INSERT INTO recommendation_publish (source_article_id,recommended_article_id,rank)
        VALUES (?,?,?)|]

-- we want to make sure we delete any old crud hanging out in the database:

deleteOldPubsStmt :: Query
deleteOldPubsStmt = 
   [sql|DELETE FROM recommendation_publish WHERE source_article_id=?|]

insertPub :: Connection -> Integer -> [Integer] -> IO ()
insertPub conn srcId recIds = undefined

-- n.b.: the rank is determined by order, so:
-- recId1 rank is 1, recId2 rank is 2, ...

{-- BONUS -----------------------------------------------------------------

write an app that takes srcId, recIds and saves that info to the database
--}

main' :: [String] -> IO ()
main' artIds = undefined
