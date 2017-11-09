{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M11.D09.Solution where

{--
Okay, today, back to reading from the database. We have a new set of articles 
with new recommendations, but with just the id,title, and score. We have the 
keyphrases (which we will need to store in the database), so we need the other 
data contained in our database. We also have the data structure we need as a 
result, so: let's go get us some data!
--}

import Control.Arrow ((&&&))
import qualified Data.Map as Map
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection

import Y2017.M11.D03.Solution
import Y2017.M11.D06.Solution
import Y2017.M11.D07.Solution
import Y2017.M11.D08.Solution

-- here's our SQL to get our structure

recommendsStmt :: Query
recommendsStmt = [sql|SELECT id,title,full_text,author,publish_dt FROM article
                      WHERE id IN ?|]

fetchRecommend :: Connection -> [Integer] -> IO [Recommend]
fetchRecommend conn = query conn recommendsStmt . Only . In

-- of course, we need the row instance of Recommend:

instance FromRow Recommend where
   fromRow = Rec <$> (show <$> (field :: RowParser Integer))
                 <*> field <*> field <*> field <*> field

-- Now that we have our data on the recommendations, the scores and the keywords
-- (see imports above): generate the JSON as before

-- step 1: read in article-set.csv using definitions from Y2017.M11.D07.Exercise

articleSet :: FilePath
articleSet = "Y2017/M11/D09/article-set.csv"

{--
>>> scores <- readScoreFile articleSet

step 2: read in the keywords, as before, using Y2017.M11.D03.Exercise's 
definitions

>>> kws <- readCompressedKeywords "Y2017/M11/D03/refinedEr_kws.txt.gz" 

step 3: read in the article-data using the SQL here.

>>> connectInfo 
ConnectInfo {connectHost = "...",...}
>>> conn <- connect it
>>> arts <- fetchRecommend conn (Data.Map.keys scores)
>>> length arts
26

step 4: generate the pretty JSON, using Y2017.M11.D08.Exercise's definitions

(first I need to convert the arts into a Map:)

>>> recs = Map.fromList (map (((read . recIdx) :: Recommend -> Integer) &&& id) arts)

(now I can JSONify everyting)

>>> saveRecs "Y2017/M11/D09/new-recs.json" kws (marry recs scores) 

There you go!
--}
