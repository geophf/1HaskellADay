{-# LANGUAGE OverloadedStrings #-}

module Y2018.M06.D19.Exercise where

{--
Same, but different. Today we are going to look at the non-duplicate articles
of the Virginian-Pilot: those articles that have the same UUID but are 
materially different. This time, however, these articles have their full text
and are formatted as JSON.
--}

import Data.Aeson
import Data.Map (Map)
import Data.Time

type UUID = String

data Article = Art { uuid :: UUID, id :: Integer, published :: Day,
                     updated :: Maybe Day, title, text :: String }
   deriving (Eq, Show)

exDir, artJSON :: FilePath
exDir = "Y2018/M06/D19/"
artJSON = "non-duplicate-articles.json"

instance FromJSON Article where
   parseJSON art = undefined

readNonDuplicatedArticles :: FilePath -> IO [Article]
readNonDuplicatedArticles file = undefined

-- as before group these by article id:

groupArticles :: [Article] -> Map UUID [Article]
groupArticles arts = undefined

-- how many UUIDs are there? How many articles are there?

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
