{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M09.D29.Exercise where

{--
Continuing in the name-theme and in the ETL-theme, let's combine the two!

You see from the results yesterday that the unprocessed names are stored
in the database in the article table. The persons are stored in a person table
which is joined to the article table via article_person allowing for any number of
people to be associated with an article. We're going to parse the names we store
in the article table and populate the person table and the join table today.

SO! ETL PROCESS!

Today, we're going to read the raw names, with their indices, from the article 
table in the data store, parse those raw names, then store those names in a new 
data table with the referenced raw name index.

YAY!

(That's one way to go about it, another way is to store the article, parse the
names from the raw text then store the names at the same time.)
--}

import Data.Map (Map)
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection (connectInfo)

import Y2017.M09.D20.Solution (inserter)
import Y2017.M09.D26.Exercise
import Y2017.M09.D27.Exercise

-- First off, we need to read the raw names with an index from the article table

-- there's not a one-to-one mapping of the Y2017.M09.D25.Exercise.Article and
-- the article table, so we create that one-to-one mapping here

data ArticleProxy =
   AP { articleIdx :: Int,
        title      :: Maybe String,
        author     :: Maybe String,
        publishDt  :: Maybe Day,
        abstract   :: Maybe String,
        url        :: Maybe FilePath,
        section    :: Maybe String,
        fulleTxt   :: String,
        updateDt   :: Maybe Day,
        people     :: Maybe String }
   deriving (Eq, Ord, Show)

instance FromRow ArticleProxy where 
   fromRow = undefined

-- using the following query:

selectArticleStmt :: Query
selectArticleStmt = [sql|SELECT * FROM article|]

-- [sql|SELECT (id, people) from article|] -- this returns an opaque 'record'

selectArticle :: Connection -> IO [ArticleProxy]
selectArticle conn = undefined

article2RawNames :: ArticleProxy -> Maybe RawNames
article2RawNames artProx = undefined

-- n.b.: You see what we're doing here. There's no concept of RawNames now in 
-- the database entity-relation model (the ER-model), just articles with a 
-- people field. But we're extracting the RawNames values from the article 
-- table. ETL lolneatness.

-- Okay, now that we've got the raw names, convert them into a meta-person
-- value with the raw-name id and then the parsed person value:

data IxPerson = IPers { rawNameIdx :: Integer, pers :: Person }
   deriving (Eq, Ord, Show)

raw2persons :: RawNames -> [IxPerson]
raw2persons rawnames = undefined

-- now with this insert statement:

insertPersStmt :: Query
insertPersStmt =
   [sql|INSERT INTO person (source_txt,family_name,given_name,rest)
        VALUES (?,?,?,?) returning id|]

-- construct a ToRow instance of IxPerson

instance ToRow IxPerson where
   toRow pers = undefined

-- and then insert the peoples into the database...

data Index = Idx { idx :: Int }
   deriving (Eq, Ord, Show)

instance FromRow Index where
   fromRow = undefined

insertPers :: Connection -> [IxPerson] -> IO [Index]
insertPers conn peeps = undefined

-- the documentation on Database.PostgreSQL.Simple shows how to return
-- values from a database statement.

-- Now we need to connect the person to the article in the join table:

data ArticlePersonJoin = APJ { articleId, personId :: Int }
   deriving (Eq, Ord, Show)

joinArticlePersonIds :: IxPerson -> Index -> ArticlePersonJoin
joinArticlePersonIds ixpWithArticleId persId = undefined

instance ToRow ArticlePersonJoin where
   toRow (APJ aid pid) = undefined

insertArtPersJoinStmt :: Query
insertArtPersJoinStmt =
   [sql|INSERT INTO article_person (article_id,person_id) VALUES (?,?)|]

insertArtPersJoin :: Connection -> [ArticlePersonJoin] -> IO ()
insertArtPersJoin conn joins = undefined

-- How many people did you insert for the article in the database?
-- How many joins were there?

-- and have much rejoicing! YAY! *throws confetti

-- Now there's the question of how to ensure we only insert a name once, but 
-- then that gets into name-matching algorithms, and I'm going to punt on that
-- for now.
