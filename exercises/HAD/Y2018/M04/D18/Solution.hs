{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M04.D18.Solution where

-- Picking up where we left off on the World Policy Journal articles, see, e.g.:

import Y2018.M04.D02.Solution

-- I realized I never stored the Raw JSON. We need to do that, in case we mess
-- up along the way, we can always go back to the source JSON and see what went
-- wrong.

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection
import Store.SQL.Util.Indexed

-- you can work either with the json stored locally:

import Y2018.M04.D02.Solution

-- or you can download some fresh articles from the REST endpoint:

import Y2018.M04.D13.Solution

data JSONString = S' String

instance ToRow JSONString where
   toRow (S' s) = [toField s]

insertJSONs :: Connection -> [Value] -> IO [Index]
insertJSONs conn = returning conn jsonStmt . map (S' . BL.unpack . encodePretty)

jsonStmt :: Query
jsonStmt = [sql|INSERT INTO article_json (json) VALUES (?) returning id|]

-- no join table here, as it's a one-to-one relationship, but we do need the
-- id as the json_id for inserting the eventual parsed article.

-- read in a set of posts (see D02) and store the source / raw article JSON.

-- How many articles did you insert?

{--
>>> (Left pack) <- readPacket 1
>>> (Pack arts) = pack
>>> length arts
100
>>> withConnection WPJ (flip insertJSONs arts >=> print . take 5)
[Idx 1,Idx 2,Idx 3,Idx 4,Idx 5]
--}
