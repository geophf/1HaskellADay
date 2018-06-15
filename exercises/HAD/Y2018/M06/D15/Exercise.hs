module Y2018.M06.D15.Exercise where

{--
More dater today, but with a dater-fix, too!

The tab-separated file (tsv) in this directory is a report of all duplicates-
by-article_id in our article database. Your job is to find all the 'older'
ids (not article_id) of the articles and create a SQL DELETE statement to
purge the database of these pesky duplicates!

(play Mission: Improbably theme-song)
--}

import Data.Map (Map)
import Data.Time (Day)

-- below import available via 1HaskellADay git repostiory

import Control.Scan.CSV (rend) -- hint: maybe use this function

exDir, tsvFile :: FilePath
exDir = "Y2018/M06/D15/"
tsvFile = "duplicates.tsv"

-- the daters are:

type ArticleId = String
type Idx = Integer

data Article =
   Art { idx :: Idx, uuid :: ArticleId, title :: String,
         published, updated :: Maybe Day }
      deriving (Eq, Show)

readArticleDuplicates :: FilePath -> IO [Article]
readArticleDuplicates dups = undefined

{--
Given the following ordering-precedence:

published
updateed
idx

define the Ord instance of Article
--}

instance Ord Article where
   compare a b = undefined

-- now that you have the Ord instance, sort the articles in order by article_id

sortedArticles :: [Article] -> Map ArticleId [Article]
sortedArticles arts = undefined

-- and from that sorting, write the following SQL DELETE statement that deletes
-- duplicates of article_id that are 'older' that the most recent-one

deleteStmt :: [Idx] -> String
deleteStmt ids = "DELETE FROM article WHERE id IN (" ++ listOut ids ++ ")"

listOut :: Show a => [a] -> String
listOut lst = undefined
