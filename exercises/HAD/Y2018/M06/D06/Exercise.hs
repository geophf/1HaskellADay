{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M06.D05.Exercise where

{--
Another day, another data structure.

We have a smart-tagging algorithm that saves its results to JSON. We want to
take those results in store them in a database. But before we do that, we need
to parse the JSON into Haskell structures because Haskell structures are ...

... cute?
--}

import Data.Aeson

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.Pivots

data Entity a = Entity { name :: String, wiki :: WikiInfo, related :: [a] }
   deriving (Eq, Show)

instance FromJSON a => FromJSON (Entity a) where
   parseJSON (Object o) = undefined

data WikiInfo = Wiki { wikiname, wikisummary :: String,
                       wikiimages :: [FilePath], wikilink :: FilePath }
   deriving (Eq, Show)

instance FromJSON WikiInfo where
   parseJSON (Object o) = undefined

instance ToRow WikiInfo where
   toRow wi = undefined

-- the entities are stored here

exDir, entitiesFile :: FilePath
exDir = "Y2018/M06/D05/"
entitiesFile = "smart_tagging.json"

readEntities :: FilePath -> IO [Entity Value]
readEntities file = undefined

-- How many entities are there?
-- What is the name of the entity that has the most related articles?
-- How many related articles does it have?

{-- PART DUEX! --------------------------------------------------------------

Into a new database, store these entities with their related wikipedia links.
--}

imgStmt :: Query
imgStmt = [sql|INSERT INTO image (url) VALUES (?) returning id|]

wikiStmt :: Query
wikiStmt = [sql|INSERT INTO background_information (source, summary,url)
                VALUES (?,?,?) returning id|]

pvtStmt :: Query
pvtStmt = [sql|INSERT INTO background_information_image
                           (background_information_id,image_id) VALUES (?,?)|]

-- inserting wikidata is a three step process

storeWikiData :: Connection -> WikiInfo -> IO Index
storeWikiData conn info =
   -- first we store the information and get the id:

   returning conn wikiStmt [info] >>= \[widx] ->

   -- then we store images and get their ids

   undefined >>= \imgidxn ->

   -- finally we store the pivoted wikidata-image set information

   undefined  >>

   return widx -- you figure out the undefined parts

-- each entity has wikidata, so store the Entity given the wikidata

entityStmt :: Query
entityStmt = [sql|INSERT INTO entity (wiki,entity) VALUES (?,?) returning id|]

-- wiki is the index of the inserted wikidata information, entity is the
-- entity name. Hint: Indexed values

-- this tagged-type data structure may be helpful:

data Tagged t = Tag t
   deriving (Eq, Ord, Show)

instance ToField t => ToRow (Tagged t) where
   toRow tag = undefined

storeEntities :: Connection -> [Entity a] -> IO [Index]
storeEntities conn ents =

-- for entity x we store the wikidata then, with the wikidata index we store
-- the entity and return the entity id

   undefined
