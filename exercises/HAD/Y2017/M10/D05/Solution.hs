{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Y2017.M10.D05.Solution where

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
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Database.PostgreSQL.Simple

-- We've got the database reads from yesterday exercise. Do that.

-- below imports available via 1HaskellADay git repository

import Data.Hierarchy
import Store.SQL.Util.Indexed
import Store.SQL.Util.Pivots

import Y2017.M10.D04.Solution

-- Now let's save out those data to JSON

instance ToJSON Subject where
   toJSON (IxV i val) = object ["id" .= i, "subject" .= val]

instance ToJSON Pivot where
   toJSON (Pvt k1 k2) = object ["article_id" .= k1, "subject_id" .= k2]

instance ToJSON ArticleSummary where
   toJSON (ArtSum i title published) =
      object ["id" .= i, "title" .= title, "publish_dt" .= published]

-- Okay, so we've enjsonified the rows of the tables, now let's enjsonify
-- the entire table!

data Table a = Table { name :: String, rows :: [a] }
   deriving (Eq, Show)

instance ToJSON a => ToJSON (Table a) where
   toJSON (Table name rows) = object [T.pack name .= rows]

-- save out the three tables as JSON to their own files

-- TIME PASSES -------------------------------------------------------------

-- Okay now do the same thing IN REVERSE! Load the tables from JSON files

instance FromJSON a => FromJSON (Table a) where
   parseJSON o = parseJSON o >>= parseKV . head . HM.toList

-- from https://stackoverflow.com/questions/42578331/aeson-parse-json-with-unknown-key-in-haskell

parseKV :: FromJSON a => (String, Value) -> Parser (Table a)
parseKV (name,list) = Table name <$> parseJSON list

-- Well, would you look at that! 'save to JSON' on SQL whatever saves ids as
-- strings. Isn't that... 'cute'! smh

instance FromJSON Subject where
   parseJSON (Object o) = IxV <$> (read <$> o .: "id") <*> o .: "subject"

instance FromJSON Pivot where
   parseJSON (Object o) = Pvt <$> (read <$> o .: "article_id")
                              <*> (read <$> o .: "subject_id")

instance FromJSON ArticleSummary where
   parseJSON (Object o) = ArtSum <$> (read <$> o .: "id")
                                 <*> o .: "title" <*> o .: "publish_dt"

-- read in your tables back
-- run your analyses
-- celebrate with Pumpkin Spice Lattes!

{--
>>> subjs <- (rows . fromJust . decode <$> BL.readFile "Y2017/M10/D05/subj.json") :: IO [Subject]
>>> length subjs
1193
>>> pivs <- (rows . fromJust . decode <$> BL.readFile "Y2017/M10/D05/art-subj.json") :: IO [Pivot]
>>> arts <- (rows . fromJust . decode <$> BL.readFile "Y2017/M10/D05/art.json") :: IO [ArticleSummary]
>>> length arts
599
--}

{-- BONUS -----------------------------------------------------------------

Once you have the grouping from yesterday, you can output the result 
graphically as:

1. 3D scatterplot of topics by date
2. Concentric circles: topics containing dates containing articles
3. N-dimentional graph of Topics x Dates x Articles

you choose, or create your own data visualization
--}

-- With the generalization of hierarchies:

data ArchiveNode = Date Day | Group Topic | Sum ArticleSummary | Archive String

instance Tuple ArchiveNode where
   toPair (Date d) = ("date", toJSON d)
   toPair (Group t) = ("topic", toJSON t)
   toPair (Sum art) = ("article", toJSON art)
   toPair (Archive arc) = ("archive", toJSON arc)
{--
instance ToJSON ArchiveNode where
   toJSON (Date d) = object ["date" .= d]
   toJSON (Group t) = object ["topic" .= t]
   toJSON (Sum art) = object ["article" .= art]
   toJSON (Archive arc) = object ["archive" .= arc]
--}

visualize :: FilePath -> Grouping -> IO ()
visualize file = BL.writeFile file . encodePretty . groupToHierarchy

groupToHierarchy :: Grouping -> Hierarchy ArchiveNode
groupToHierarchy =
   Hier (Archive "NYT Archive") . Kids . map topicArts . Map.toList

topicArts :: (Topic, Map Day [ArticleSummary]) -> Hierarchy ArchiveNode
topicArts (topic, days) =
   Hier (Group topic) (Kids (map dayArts (Map.toList days)))

dayArts :: (Day, [ArticleSummary]) -> Hierarchy ArchiveNode
dayArts (d, ts) =
   Hier (Date d) (Kids (map (flip Hier (Size 1) . Sum) ts))

{--
>>> let grp = graphTopics subjs pivs arts 
>>> length grp
976
>>> visualize "Y2017/M10/D05/topics.json" grp

{
    "children": [
        {
            "children": [
                {
                    "children": [
                        {
                            "size": 1,
                            "name": "From an Undervalued Region in France, New Energy, New Inspiration and Great Wines"
                        }
                    ],
                    "name": "2017-09-07"
                }
            ],
            "name": "19th century"
        }, ...
    ],
    "name": "NYT Archive"
}

*Looks at circles of all topics.

HOLY SHNITZELS! THAT THERE IS A LOT OF DATA!

'Tomorrow' we'll look at narrowing the view and looking at only a few topics.
--}
