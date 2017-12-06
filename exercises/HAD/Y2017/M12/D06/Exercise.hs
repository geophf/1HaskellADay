{-# LANGUAGE OverloadedStrings #-}

module Y2017.M12.D06.Exercise where

{--
So, we're filtering articles from the NYT archive by keyword. Great!

Now, we have this article/recommended articles construct in our database. We
don't want to show in our filted article set the source article nor the articles
already recommended.

So, today's Haskell problem is, given the source article id (from which you can
find the recommended article ids) filter out of the filtered article set those
given article ids.

Computer science is sorting and filtering.
--}

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Database.PostgreSQL.Simple

-- below imports available via 1HaskellADay git repository

import Control.Scan.Config
import Store.SQL.Connection (withConnection)
import Store.SQL.Util.Indexed

import Y2017.M11.D01.Exercise   -- for SpecialCharTable
import Y2017.M11.D07.Exercise   -- for Recommendation
import Y2017.M11.D21.Exercise (rec2brief)
import Y2017.M11.D24.Exercise (recs4)
import Y2017.M12.D05.Exercise   -- for recs

-- so we get our set of filtered articles with recs, and we get our recommended
-- files for the source article with recs4, so now, we need to remove the
-- Recommendation values from the result of recs that have the ids from recs4

filterMinusRecs :: Integer -> [IxValue a] -> [Recommendation] -> [Recommendation]
filterMinusRecs srcId recsIds recs = undefined

-- a neat trick to know is that PostgreSQL returns recommendations sorted.

{-- BONUS -----------------------------------------------------------------

End-to-end this. From a source article id and a set of keywords, return a 
filtered set of articles that don't contain the source article nor already
recommended articles
--}

main' :: [String] -> IO ()
main' (srcId:keywords) = undefined
