{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M11.D09.Exercise where

{--
Okay, today, back to reading from the database. We have a new set of articles 
with new recommendations, but with just the id,title, and score. We have the 
keyphrases (which we will need to store in the database), so we need the other 
data contained in our database. We also have the data structure we need as a 
result, so: let's go get us some data!
--}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection

import Y2017.M11.D03.Exercise
import Y2017.M11.D06.Exercise
import Y2017.M11.D07.Exercise
import Y2017.M11.D08.Exercise

-- here's our SQL to get our structure

recommendsStmt :: Query
recommendsStmt =
   [sql|SELECT a.id,a.title,s.summary,a.author,a.publish_dt,a.view_count
        FROM article a
        LEFT JOIN article_summary s ON s.article_id = a.id
        WHERE a.id IN ?|]

fetchRecommend :: Connection -> [Integer] -> IO [Recommend]
fetchRecommend conn ids = undefined

-- of course, we need the row instance of Recommend:

instance FromRow Recommend where
   fromRow = undefined

-- Now that we have our data on the recommendations, the scores and the keywords
-- (see imports above): generate the JSON as before

-- step 1: read in article-set.csv using definitions from Y2017.M11.D07.Exercise

articleSet :: FilePath
articleSet = "Y2017/M11/D09/article-set.csv"

-- step 2: read in the keywords, as before, using Y2017.M11.D03.Exercise's 
-- definitions

-- step 3: read in the article-data using the SQL here.

-- step 4: generate the pretty JSON, using Y2017.M11.D08.Exercise's definitions

-- There you go!
