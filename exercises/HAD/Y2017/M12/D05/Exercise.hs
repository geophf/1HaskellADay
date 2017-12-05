{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M12.D05.Exercise where

{--
So, we have indexed the archives of the NYT article set and search by 
keyword-index.

Good.

The problem is the previous infrastructure built an entire keyword-key-phrase
dictionary. That's overkill, given that you have 25,000+ keywords and 218,000+
key-phrases, and, as the archive expands, so will the keyword-key-phrase
dictionary.

Not good.

So, today's Haskell problem is this: given a set of keywords, construct a
keyword-key-phrase dictionary of only those keywords.
--}

import Control.Monad ((>=>))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay git repository

import Control.Scan.Config
import Store.SQL.Connection (withConnection)

import Y2017.M11.D01.Exercise               -- for SpecialCharTable
import Y2017.M11.D07.Exercise               -- for Recommendation
import Y2017.M11.D13.Exercise               -- for KeyWord
import Y2017.M11.D17.Exercise               -- for KWtable, IxKeys
import Y2017.M11.D20.Exercise hiding (recs) -- for kwTable', recs'
import Y2017.M11.D21.Exercise               -- for Briefs

keywordKeyphraseDict :: Connection -> [KeyWord] -> IO KWtable
keywordKeyphraseDict conn keys = undefined

-- keywordKeyphraseDict is a rewrite of Y2017.M11.D20.Exercise.kwTable
-- ... which means you have to rewrite Y2017.M11.D17.Exercise.fetchKW:

fetchKWbyKWStmt :: Query
fetchKWbyKWStmt = [sql|SELECT * from keyword where keyword IN ?|]

fetchKWbyKW :: Connection -> [KeyWord] -> IO [IxKeys]
fetchKWbyKW conn keywords = undefined

-- now we simply rewrite recs so that it uses the subset of the keyword table

recs :: Connection -> [KeyWord] -> IO [Recommendation]
recs conn keywords = undefined   -- hint: use recs' to finish off this function

{-- BONUS -----------------------------------------------------------------

Write an application that does keyword searches and returns article briefs,
rendering them as JSON (i.e.: like we did for Y2017.M11.D21.Exercise)
--}

main' :: [String] -> IO ()
main' keywords = undefined
