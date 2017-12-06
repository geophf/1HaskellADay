{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M12.D05.Solution where

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

import Y2017.M11.D01.Solution               -- for SpecialCharTable
import Y2017.M11.D07.Solution               -- for Recommendation
import Y2017.M11.D13.Solution               -- for KeyWord
import Y2017.M11.D17.Solution               -- for KWtable, IxKeys
import Y2017.M11.D20.Solution hiding (recs) -- for kwTable', recs'
import Y2017.M11.D21.Solution               -- for briefs

keywordKeyphraseDict :: Connection -> [KeyWord] -> IO KWtable
keywordKeyphraseDict conn keys = kwTable' <$> fetchKWbyKW conn keys

-- keywordKeyphraseDict is a rewrite of Y2017.M11.D20.Solution.kwTable
-- ... which means you have to rewrite Y2017.M11.D17.Solution.fetchKW:

fetchKWbyKWStmt :: Query
fetchKWbyKWStmt = [sql|SELECT * from keyword where keyword IN ?|]

fetchKWbyKW :: Connection -> [KeyWord] -> IO [IxKeys]
fetchKWbyKW conn = query conn fetchKWbyKWStmt . Only . In

-- now we simply rewrite recs so that it uses the subset of the keyword table

recs :: Connection -> [KeyWord] -> IO [Recommendation]
recs conn kws = keywordKeyphraseDict conn kws >>= recs' conn kws

{-- BONUS -----------------------------------------------------------------

Write an application that does keyword searches and returns article briefs,
rendering them as JSON (i.e.: like we did for Y2017.M11.D21.Solution)
--}

main' :: [String] -> IO ()
main' keywords = do
   homeDir <- home
   special <- readSpecialChars (homeDir ++ "/.specialChars.prop")
   withConnection (flip recs keywords >=>
      BL.putStrLn . encodePretty . map (rec2brief special))
