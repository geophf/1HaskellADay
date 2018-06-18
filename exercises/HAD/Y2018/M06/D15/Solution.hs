module Y2018.M06.D15.Solution where

{--
More dater today, but with a dater-fix, too!

The tab-separated file (tsv) in this directory is a report of all duplicates-
by-article_id in our article database. Your job is to find all the 'older'
ids (not article_id) of the articles and create a SQL DELETE statement to
purge the database of these pesky duplicates!

(play Mission: Improbably theme-song)
--}

import Control.Arrow ((&&&))

import Data.Function (on)
import Data.List (sort, groupBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Time (Day)

-- below import available via 1HaskellADay git repostiory

import Control.List (weave)
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
readArticleDuplicates =
   fmap (map (line2Art . rend '\t') . tail . lines) . readFile

line2Art :: [String] -> Article
line2Art [idx,artId,pub,upd,titl] =
   Art (read idx) artId titl (s2d pub) (s2d upd)
line2Art x = error ("Couldn't read line '" ++ show x ++ "'")

-- converts a string to a date

s2d :: String -> Maybe Day
s2d "" = Nothing
s2d d@(_:_) = Just (read d)

{--
>>> arts <- readArticleDuplicates (exDir ++ tsvFile)
>>> length arts
4723
>>> take 2 arts
[Art {idx = 275322, uuid = "08cf5409-52c9-59e7-a9ba-a46f6506ffb6", 
      title = "Live scoreboard: High school playoffs", 
      published = Just 2018-06-08, updated = Just 2018-06-08},
 Art {idx = 275116, uuid = "46a8198e-1653-5bb8-b992-8bb4dd5c67c1", 
      title = "Hurricanes come, but the residents of Hatteras Island wouldn't dream of leaving.", 
      published = Just 2018-06-05, updated = Just 2018-06-06}]
Given the following ordering-precedence:

uuid
published
updateed
idx

define the Ord instance of Article

Note:

>>> Just 5 > Just 3
True
>>> Just 5 > Nothing
True

is the behavior I want
--}

instance Ord Article where
   compare (Art i1 a1 _ p1 u1) (Art i2 a2 _ p2 u2) =
      compare a1 a2 <> compare p1 p2 <> compare u1 u2 <> compare i1 i2

-- I need a compare-but-if-it-is-equal-then-do-the-subcompare-function ...
-- good thing Ordering has a Monoid instance

-- now that you have the Ord instance, sort the articles in order by article_id

sortedArticles :: [Article] -> Map ArticleId [Article]
sortedArticles =
   Map.fromList . map (uuid . head &&& id) . groupBy ((==) `on` uuid) . sort

-- Some helper functions for showing results

st :: Show a => a -> String
st = take 10 . show

-- 'st' for 'show 10'

mb2s :: Show a => Maybe a -> String
mb2s = maybe "" st

-- 'mb2s' for 'maybe to string'

nt :: Article -> String
nt (Art i a t p u) =
   "Art " ++ weave [show i, take 20 a, take 20 t, mb2s p, mb2s u]

-- 'nt' for something I don't remember.

-- I'M PROGRAMMING IN FORTH AGAIN!

{--
>>> mapM_ putStrLn . take 5 . concat . Map.elems $ Map.map (map nt) (sortedArticles arts)
Art 220829,0057e006-ec63-58fb-a,Seriously, who needs,2008-04-28,
Art 220837,0057e006-ec63-58fb-a,Seriously, who needs,2008-04-28,
Art 214131,00d45ed5-8c1d-53d8-8,Virginia Beach man d,2008-07-10,
Art 216001,00d45ed5-8c1d-53d8-8,Virginia Beach man d,2008-07-10,
Art 214908,00d70c0a-6143-5923-a,Fire damages Newport,2008-06-24,2015-11-06

>>> mapM_ (putStrLn . nt) $ take 5 arts
Art 275322,08cf5409-52c9-59e7-a,Live scoreboard: Hig,2018-06-08,2018-06-08
Art 275116,46a8198e-1653-5bb8-b,Hurricanes come, but,2018-06-05,2018-06-06
Art 274844,e5decfb4-1cc5-52f8-a,Is your startup read,2018-05-30,2018-05-31
Art 274502,fe997d65-9969-5b4a-8,Rankings | Top 10 ba,2018-05-21,
Art 274497,fa8c30e6-b8e4-5009-b,Final rankings: Top ,2018-05-21,2018-05-21

So, the sorting worked. Are there any elements of the map that have 1 or less
articles?

>>> Map.filter ((< 2) . length) (sortedArticles arts)
fromList []

Nope. So we're good there. Let's finish up.
--}

-- and from that sorting, write the following SQL DELETE statement that deletes
-- duplicates of article_id that are 'older' that the most recent-one

deleteStmt :: [Idx] -> String
deleteStmt ids = "DELETE FROM article WHERE id IN (" ++ listOut ids ++ ")"

listOut :: Show a => [a] -> String
listOut = weave . map show

duplicateIds :: [Article] -> [Idx]
duplicateIds = map idx . tail . reverse

{--
>>> take 75 . deleteStmt . concat . Map.elems . Map.map duplicateIds
            $ sortedArticles arts
"DELETE FROM article WHERE id IN (220829,214131,214908,214684,214799,213243,"
--}
