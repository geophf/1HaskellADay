{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Y2017.M10.D05.Exercise where

{--
Yesterday, I said: "Go out to the database, get some data, analyze it and chart
it." Well, that's all good if you can work on the problem when you have access
to the internet at large to retrieve your data from the DaaS, but let's say,
hypothetically speaking, that you're behind a firewall, so you only have tiny
slots of time when you go out for that Pumpkin Spiced Latte with your friendies
to access those data. You can't write code, get the data, analyze the data and
then plot the results over just one 'PSL' (The TLAs are taking over smh).

So, what's the solution? Order a second PSL? But then that contributes to the
obesity pandemic, and I don't want that on my resume, thank you very much.

And then there's the credit ratings to consider.

(somebody cue the theme to the movie 'Brazil')

So, the data sets we're talking about aren't petabytes nor exobytes, so why not
just retain those data locally so the development and analyses can be done
off-line or behind the firewall?

Why not, indeed!

Today's Haskell problem: read the rows of data from the article, subject, and
article_subject tables, save them locally as JSON, do some magic hand-waving
or coffee-drinking while you analyze those data off-line, then read in the
data from the JSON store.
--}

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import Data.Map (Map)
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Database.PostgreSQL.Simple

-- We've got the database reads from yesterday exercise. Do that.

-- below imports available via 1HaskellADay git repository

import Graph.D3.CirclePacking
import Store.SQL.Util.Pivots

import Y2017.M10.D04.Exercise

-- Now let's save out those data to JSON

instance ToJSON Subject where
   toJSON subj = undefined

instance ToJSON Pivot where
   toJSON piv = undefined

instance ToJSON ArticleSummary where
   toJSON artsum = undefined

-- Okay, so we've enjsonified the rows of the tables, now let's enjsonify
-- the entire table!

data Table a = Table { name :: String, rows :: [a] }
   deriving (Eq, Show)

instance ToJSON a => ToJSON (Table a) where
   toJSON table = undefined

-- save out the three tables as JSON to their own files

-- TIME PASSES -------------------------------------------------------------

-- Okay now do the same thing IN REVERSE! Load the tables from JSON files

instance FromJSON a => FromJSON (Table a) where
   parseJSON obj = undefined

parseKV :: FromJSON a => (String, Value) -> Parser (Table a)
parseKV (name,list) = undefined

{--
Hint: parsing tables is a bit tricky because the key to the values is the
table name. See:

https://stackoverflow.com/questions/42578331/aeson-parse-json-with-unknown-key-in-haskell

for a discussion on parsing unknown keys
--}

instance FromJSON Subject where
   parseJSON obj = undefined

instance FromJSON Pivot where
   parseJSON obj = undefined

instance FromJSON ArticleSummary where
   parseJSON obj = undefined

-- read in your tables back
-- run your analyses
-- celebrate with Pumpkin Spice Lattes!

{-- BONUS -----------------------------------------------------------------

Once you have the grouping from yesterday, you can output the result
graphically as:

1. 3D scatterplot of topics by date
2. Concentric circles: topics containing dates containing articles
3. N-dimentional graph of Topics x Dates x Articles

you choose, or create your own data visualization
--}

visualize :: Grouping -> IO ()
visualize groups = undefined

groupToHierarchy :: Grouping -> Hierarchy
groupToHierarchy = Hier "NYT Archive" . undefined

topicArts :: (Topic, Map Day [Title]) -> Hierarchy
topicArts row = undefined

dayArts :: (Day, [Title]) -> Hierarchy
dayArts = undefined
