{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M06.D06.Solution where

{--
Another day, another data structure.

We have a smart-tagging algorithm that saves its results to JSON. We want to
take those results in store them in a database. But before we do that, we need
to parse the JSON into Haskell structures because Haskell structures are ...

... cute?
--}

import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromJust)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.Pivots

data Entity a = Entity { name :: String, wiki :: WikiInfo, related :: [a] }
   deriving (Eq, Show)

instance FromJSON a => FromJSON (Entity a) where
   parseJSON (Object o) =
      Entity <$> o .: "Name" <*> o .: "Wiki" <*> o .: "Related Articles"

data WikiInfo = Wiki { wikiname, wikisummary :: String,
                       wikiimages :: [FilePath], wikilink :: FilePath }
   deriving (Eq, Show)

instance FromJSON WikiInfo where
   parseJSON (Object o) =
      Wiki <$> o .: "Name"   <*> o .: "Summary" 
           <*> o .: "Images" <*> o .: "Link"

-- the entities are stored here

exDir, entitiesFile :: FilePath
exDir = "Y2018/M06/D05/"
entitiesFile = "smart_tagging_new.json"

data Entities a = Ents { ents :: [Entity a] }

instance FromJSON a => FromJSON (Entities a) where
   parseJSON (Object o) = Ents <$> o .: "Entities"

readEntities :: FilePath -> IO [Entity Value]
readEntities = fmap (ents . fromJust . decode) . BL.readFile

{--
>>> json <- readEntities (exDir ++ entitiesFile)
>>> length json
12
--}

{-- PART DUEX! --------------------------------------------------------------

Into a new database, store these entities with their related wikipedia links.
--}

imgStmt :: Query
imgStmt = [sql|INSERT INTO image (url) VALUES (?) returning id|]

wikiStmt :: Query
wikiStmt = [sql|INSERT INTO background_information (source,summary,url)
                VALUES (?,?,?) returning id|]

pvtStmt :: Query
pvtStmt = [sql|INSERT INTO background_information_image
                           (background_information_id,image_id) VALUES (?,?)|]

instance ToRow WikiInfo where
   toRow (Wiki n s i l) =  map toField [n,s,l]

-- inserting wikidata is a three step process

storeWikiData :: Connection -> WikiInfo -> IO Index
storeWikiData conn info =
   -- first we store the information and get the id:

   returning conn wikiStmt [info] >>= \[widx] ->

   -- then we store images and get their ids

   storeImages conn info >>= \imgidxn ->

   -- finally we store the pivoted wikidata-image set information

   executeMany conn pvtStmt (zipWith joinValue (repeat widx) imgidxn)  >>
   return widx

-- each entity has wikidata, so store the Entity given the wikidata

entityStmt :: Query
entityStmt = [sql|INSERT INTO entity (wiki,entity) VALUES (?,?) returning id|]

-- wiki is the index of the inserted wikidata information, entity is the
-- entity name. Hint: Indexed values

-- this tagged-type data structure may be helpful: it converts a ToField value
-- e.g. a String to a ToRow value

data Tagged t = Tag t
   deriving (Eq, Ord, Show)

instance ToField t => ToRow (Tagged t) where
   toRow (Tag t) = [toField t]

storeImages :: Connection -> WikiInfo -> IO [Index]
storeImages conn = returning conn imgStmt . map Tag . wikiimages

entStmt :: IxValue (Tagged String) -> Query
entStmt (IxV ix (Tag n)) =
   Query (B.pack ("INSERT INTO entity (wiki,entity,publication) VALUES ("
               ++ show ix ++ ",'" ++ n ++ "',3) returning id"))

wikiEnt2idx :: Index -> Entity a -> IxValue (Tagged String)
wikiEnt2idx (Idx ix) = IxV ix . Tag . name

storeEntities :: Connection -> [Entity a] -> IO [Index]
storeEntities conn =

-- for entity x we store the wikidata then, with the wikidata index we store
-- the entity and return the entity id

   fmap concat . mapM (\ent -> storeWikiData conn (wiki ent) >>= \widx ->
                               query_ conn (entStmt (wikiEnt2idx widx ent)))

{--
>>> withConnection ENTITIES (\conn -> storeEntities conn json >>=
                              putStrLn . ("I put entities in rows " ++) . show)
I put entities in rows [Idx 2,Idx 3,Idx 4,Idx 5,Idx 6,Idx 7,Idx 8,Idx 9,
                        Idx 10,Idx 11,Idx 12,Idx 13]
--}
