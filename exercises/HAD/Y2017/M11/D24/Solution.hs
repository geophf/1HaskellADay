{-# LANGUAGE OverloadedStrings, QuasiQuotes, ScopedTypeVariables #-}

module Y2017.M11.D24.Solution where

{--
Let's say, oh, that you're in a start-up, and you would like to, oh, return
a list of articles associated with a source article as JSON from a PostgreSQL
database.

Hypothetically.

For example.
--}

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow

-- below imports available via 1HaskellADay git repository.

import Control.Scan.Config
import Store.SQL.Connection
import Store.SQL.Util.Indexed

import Y2017.M11.D01.Solution   -- for SpecialCharTable
import Y2017.M11.D03.Solution   -- for Strength
import Y2017.M11.D06.Solution   -- for Value
import Y2017.M11.D09.Solution   -- for fetchRecommend
import Y2017.M11.D21.Solution   -- for Brief

{--
This is the Brief format, but instead of filtering down to a set of articles,
these articles, and their scores are already provided for you in the database.

So this exercise eta-reduces to writing a FromRow-ish instance on Brief, 
extracting data from multiple tables then cleaning up special characters as
we go.
--}

recs4Stmt :: Query
recs4Stmt = [sql|SELECT recommended_article_id,hindsight_score
                 FROM recommendation WHERE for_article_id=?|]

recs4 :: Connection -> Integer -> IO [IxValue (Value Strength)]
recs4 conn artId = query conn recs4Stmt [artId]

{--
Why am I doing this when I did it better in D09?

-- now, we need to get the rest of the data, given the recommendation ids,
-- from the article table

articleDataStmt :: Query
articleDataStmt =
   [sql|SELECT id,publish_dt,title,full_text FROM article
                       WHERE id IN ?|]
--}

-- then we marry the IxValue with the data returned here to get a Brief

articleData :: SpecialCharTable -> Connection -> [IxValue (Value Strength)] 
            -> IO [Brief]
articleData chars conn artIds =
   let strengths = Map.fromList (map ix2tup artIds) in
   map (\(idx,tit,summ,_auth :: Maybe String,dt,vc) ->
         Summarized idx dt (refineString chars tit) (summarize chars <$> summ)
                    vc (strengths Map.! idx))
        <$> query conn recommendsStmt (Only (In (map idx artIds)))

-- now we have to return those as JSON ... but we already know how to do that.

{-- BONUS -----------------------------------------------------------------

So, create an app that from an article id returns the set of recommended 
articles as JSON
--}

main' :: [String] -> IO ()
main' [artId] =
   let art = read artId in
   do homeDir <- home
      chars <- readSpecialChars (homeDir ++ "/.specialChars.prop")
      withConnection (\conn ->
         recs4 conn art                          >>=
         articleData chars conn . (IxV art QRY:) >>=
         BL.putStrLn . encodePretty)
main' _ = putStrLn (unlines ["", "start <article id>","",
   "\tDisplays the recommended articles for <article id>", ""])
