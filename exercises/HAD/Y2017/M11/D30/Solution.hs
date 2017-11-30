{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M11.D30.Solution where

{--
Today is the dual of yesterday's problem: we have the recommended articles to
be published in the database already, now, from some source article id, we need
to read out the associated recommended article ids and their ranks, and then
materialize and return a set of article briefs-as-JSON.
--}

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay git repository

import Control.Scan.Config
import Store.SQL.Connection
import Store.SQL.Util.Indexed

import Y2017.M11.D01.Solution  -- for SpecialCharTable
import Y2017.M11.D03.Solution  -- for Strength-type
import Y2017.M11.D06.Solution  -- for Value-type
import Y2017.M11.D21.Solution  -- for Brief-type
import Y2017.M11.D24.Solution  -- for loading Briefs from the database
import Y2017.M11.D29.Solution  -- for publish recommend article info

fetchToBePublishedStmt :: Query
fetchToBePublishedStmt =
   [sql|SELECT source_article_id,recommended_article_id,rank
        FROM recommendation_publish WHERE source_article_id=?|]

fetchPublish :: Connection -> Integer -> IO [Publish]
fetchPublish conn srcId = query conn fetchToBePublishedStmt [srcId]

-- and to-be-published info contains the kernel of article briefs we'll return

-- Oh, and P.S.: don't forget the brief of the source article

-- so we have to define Publish -> IxValue (Value Strength) where the rank is
-- the strength and the source article has value QRY

pub2Val :: Publish -> IxValue (Value Strength)
pub2Val (Pub _ rec rnk) = IxV rec (VAL (fromIntegral rnk))

-- with that we can use Y2017.M11.D24.Solution.articleData to get our briefs
-- (we have to load the special character table along the way ...)

briefs :: SpecialCharTable -> Connection -> [Publish] -> IO [Brief]
briefs _ _ [] = return []
briefs chars conn list@(Pub src _ _:_) =
   articleData chars conn (map pub2Val list)

-- don't forget the source article!

{-- BONUS -----------------------------------------------------------------

Create an app that, from a source article id, outputs the briefs of the 
recommended articles to be published with it as JSON.
--}

main' :: [String] -> IO ()
main' [srcid] =
   do homeDir <- home
      chars <- readSpecialChars (homeDir ++ "/.specialChars.prop")
      withConnection (\conn ->
         fetchPublish conn (read srcid) >>= briefs chars conn >>=
         BL.putStrLn . encodePretty)
main' _ = putStrLn (unlines ["", "printer <artId>", "", "\tGives the article "
             ++ "and the recommended articles to be published as JSON", ""])
