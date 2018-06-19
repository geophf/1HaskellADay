{-# LANGUAGE OverloadedStrings #-}

module Y2018.M06.D19.Solution where

{--
Same, but different. Today we are going to look at the non-duplicate articles
of the Virginian-Pilot: those articles that have the same UUID but are 
materially different. This time, however, these articles have their full text
and are formatted as JSON.
--}

import Control.Arrow ((&&&))

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function (on)
import Data.List (sort, groupBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Time
import qualified Data.Text as T

type UUID = String

data Article = Art { uuid :: UUID, idx :: Integer, published :: Day,
                     updated :: Maybe Day, title, text :: String }
   deriving (Eq, Ord, Show)

exDir, artJSON :: FilePath
exDir = "Y2018/M06/D19/"
artJSON = "non-duplicate-articles.json"

instance FromJSON Article where
   parseJSON (Object o) =
      Art <$> o .: "article_id" <*> (read <$> o .: "id")
          <*> o .: "publish_dt" <*> (obj2mbDt <$> o .: "update_dt")
          <*> o .: "title"      <*> o .: "full_text"
         where obj2mbDt Null = Nothing
               obj2mbDt (String s) = Just (read (T.unpack s))

data ArticleSet = AS { arts :: [Article] }

instance FromJSON ArticleSet where
   parseJSON (Object o) = AS <$> o .: "non-duplicate-articles"

readNonDuplicatedArticles :: FilePath -> IO [Article]
readNonDuplicatedArticles = fmap (arts . fromJust . decode) . BL.readFile

{--
>>> arts <- readNonDuplicatedArticles (exDir ++ artJSON)
>>> take 5 . drop 30 $ map updated arts
[Nothing,Nothing,Nothing,Just 2018-03-20,Nothing]
--}

-- as before group these by article id:

groupArticles :: [Article] -> Map UUID [Article]
groupArticles =
   Map.fromList . map (uuid . head &&& id) . groupBy ((==) `on` uuid) . sort

-- how many UUIDs are there? How many articles are there?

{--
>>> take 2 . Map.keys $ groupArticles arts
["08cf5409-52c9-59e7-a9ba-a46f6506ffb6","098e42bc-368f-11e8-a40a-03507fc5b471"]

>>> length $ groupArticles arts
30

>>> length arts
91
--}

{-- BONUS -----------------------------------------------------------------

Okay, this is fun. We see that some of these articles are materially different.

No duh.

Classify these articles into sets, one set being the class of UUIDs that have
materially different articles, and the other set being the class of UUIDs that
have articles that are 'kinda' the same.

Let'r rip: Bayesian analysis, clustering, cosine similarity, or the google:
'artificial' artificial intelligence (favored throughout the corporate world).
--}

data Cluster = TheStructureThatWorksForYouHere

materially :: Traversable t => Map UUID [Article] -> t Cluster
materially uuids = undefined

-- Eh. I'm looking at wordnet and cosine similarity. Maybe tomorrow.
