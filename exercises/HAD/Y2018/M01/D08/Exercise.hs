{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M01.D08.Exercise where

{--
Okay, one more, then a thought on generalization.

Friday, we looked at sections classifying newspaper articles as a graph, but
now we want to store unique sections in a table and relate those sections back
to the articles they classify.

Like before. So.
--}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- below imports available via 1HaskellADay git repository

import Data.MemoizingTable (MemoizingS)
import qualified Data.MemoizingTable as MT

import Store.SQL.Connection (withConnection)
import Store.SQL.Util.Indexed
import Store.SQL.Util.Pivots

import Y2017.M12.D20.Exercise -- for Block
import Y2017.M12.D27.Exercise -- for DatedArticle
import Y2017.M12.D29.Exercise -- for filtering out AP articles
import Y2018.M01.D02.Exercise -- for keywords and etl
import Y2018.M01.D04.Exercise -- for Author

-- Okay, sections are like subjects are like keywords, but not quite like
-- authors (not quite). In Prolog we call a simple value with typing information
-- a tagged-type. We don't have those here, but maybe we should?

-- first thought on generalization

data Section = Sect { section :: String }
   deriving (Eq, Ord, Show)

fetchSectionStmt :: Query
fetchSectionStmt = [sql|SELECT * FROM section|]

fetchSections :: Connection -> IO [IxValue Section]
fetchSections conn = undefined

instance FromRow Section where
   fromRow = undefined

-- okay, we can get sections previously stored. Fetch them and populate a
-- memoizing table with them.

-- Now, from an article, extract its sections. That's easy enough.

-- Now we put that all together and populate the section table with new
-- section information

storeSections :: Connection -> [IxValue (DatedArticle a)] -> IO ()
storeSections conn arts = undefined

insertSectionStmt :: Query
insertSectionStmt = [sql|INSERT INTO section (section) VALUES (?) returning id|]

insertSections :: Connection -> [Section] -> IO [Index]
insertSections conn sects = undefined

insertArtSectPivotStmt :: Query
insertArtSectPivotStmt =
   [sql|INSERT INTO article_section (article_id,section_id) VALUES (?,?)|]

-- hint: use Pivots module for pivot inserts

instance ToRow Section where
   toRow sect = undefined

-- with storeSections defined, define a new storeAncilliary to extract and
-- store values for keywords, authors and sections.

storeAncilliary :: Connection -> [IxValue (DatedArticle Authors)] -> IO ()
storeAncilliary conn ixarts = undefined

-- and apply this storeAncilliary to the etl function, and you've captured
-- the information we're interested in with the articles sampled.

-- Hint: use pa to process the articles

{-- BONUS -----------------------------------------------------------------

Keywords, Authors, and sections. All three follow the same pattern:

* fetch previously stored values
* memoize
* materials values from article set, see which values are new
* store new values, relate all values.

If we are following the same pattern, is there a generalization of the
functions for all of this, so we can have one function that does the work
for any memoize-y type? What would this function look like?
--}

memoizeStore :: ToRow a => Connection -> [IxValue (DatedArticle Authors)]
             -> (IxValue (DatedArticle Authors) -> a)
             -> MemoizingS IO Integer b ()
memoizeStore conn ixarts getter = undefined

-- something like this? Or something different?
