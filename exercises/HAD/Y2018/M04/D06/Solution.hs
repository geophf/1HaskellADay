{-# LANGUAGE QuasiQuotes, OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Y2018.M04.D06.Solution where

import Control.Arrow (first)

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Functor (void)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- below imports available via 1HaskellADay git repository

import Data.LookupTable

import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.LookupTable

{--
Okay, we saved authors to the database, now we're going to save categories
to the database. But what are the categories?

I gotcher categories right here, fam:
--}

cats :: FilePath
cats = "Y2018/M04/D06/cats.json"

data Cat = Cat { mouse :: String }

instance ToRow Cat where
   toRow (Cat s) = [toField s]

type Category = IxValue Cat

instance FromJSON Category where
   parseJSON (Object o) = IxV <$> o .: "id" <*> (Cat <$> o .: "name")

readCats :: FilePath -> IO [Category]
readCats file = fromMaybe [] . decode <$> BL.readFile file

{--
>>> json <- readCats cats
>>> length json
10
>>> head json
IxV {ix = 7574, val = "African Angle"}
--}

-- now convert the list of categories to a LookupTable

cats2lk :: [Category] -> LookupTable
cats2lk = Map.fromList . map (first (unmetadataize . mouse) . swap . ix2tup)

{--
>>> cats2lk json
{("African Angle",7574),("Arctic in Context",7575),("Arts-Policy",7576),
 ("Big Question",7577),("Citizenship &amp; Identity",7578),("Coda Story",7579),
 ("Cuban Reset",7580),("Culture",7604),("Economy",7603),
 ("Elections &amp; Institutions",7581)]

... okay. seriously? SERIOUSLY? 

Elections &amp; Institutions

Nice. I'm storing my data with formatting so that it renders searching against
it a crap-shoot.

'&amp;'

That's just ... great.

Added 'unmetadataize' to get the '&amp;' behind me.
--}

convertAwayTheStupid :: String -> String
convertAwayTheStupid x = if x == "&amp;" then "&" else x

unmetadataize :: String -> String
unmetadataize = unwords . map convertAwayTheStupid . words

-- And, finally, store the lookup table into the database

catStmt :: Query
catStmt = [sql|INSERT INTO category VALUES (?, ?)|]

insertCats :: Connection -> LookupTable -> IO ()
insertCats conn =
   void . executeMany conn catStmt . map (uncurry IxV . swap . first Cat) . Map.toList
