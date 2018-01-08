{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M01.D08.Solution where

{--
Okay, one more, then a thought on generalization.

Friday, we looked at sections classifying newspaper articles as a graph, but
now we want to store unique sections in a table and relate those sections back
to the articles they classify.

Like before. So.
--}

import Control.Monad (void)
import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Set as Set

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- below imports available via 1HaskellADay git repository

import qualified Data.MemoizingTable as MT

import Store.SQL.Connection (withConnection)
import Store.SQL.Util.Indexed
import Store.SQL.Util.Pivots

import Y2017.M12.D20.Solution -- for Block
import Y2017.M12.D27.Solution hiding (pa) -- for DatedArticle
import Y2017.M12.D29.Solution hiding (etl) -- for filtering out AP articles
import Y2018.M01.D02.Solution hiding (storeAncilliary) -- for keywords and etl
import Y2018.M01.D04.Solution hiding (storeAncilliary) -- for Author

-- Okay, sections are like subjects are like keywords, but not quite like
-- authors (not quite). In Prolog we call a simple value with typing information
-- a tagged-type. We don't have those here, but maybe we should?

-- first thought on generalization

data Section = Sect { section :: String }
   deriving (Eq, Ord, Show)

fetchSectionStmt :: Query
fetchSectionStmt = [sql|SELECT * FROM section|]

fetchSections :: Connection -> IO [IxValue Section]
fetchSections conn = query_ conn fetchSectionStmt

instance FromRow Section where
   fromRow = Sect <$> field

-- okay, we can get sections previously stored. Fetch them and populate a
-- memoizing table with them.

-- Now, from an article, extract its sections. That's easy enough.

-- Now we put that all together and populate the section table with new
-- section information

storeSections :: Connection -> [IxValue (DatedArticle Authors)] -> IO ()
storeSections conn =

-- we write our storeSections using the memoizeStore from the bonus:

   memoizeStore conn fetchSections insertSections insertArtSectPivotStmt
                (map Sect . sections)

{--
   fetchSections conn >>= \presects ->
   let memtable = MT.start (map ix2tup presects)
       (ids,arts) = unzip (map ix2tup ixarts)
       stat = execState (zipWithM_ MT.triageM ids (map sectionf arts))
                                   (memtable,Map.empty)
       substate = Set.toList (MT.newValues (fst stat))
   in  insertSections conn substate >>= \ixsec ->
   let table = MT.update (zip (map idx ixsec) substate) (fst stat)
   in  void (executeMany conn insertArtSectPivotStmt
                 (evalState buildPivots (table, snd stat)))
      where sectionf = map Sect . sections

--}

insertSectionStmt :: Query
insertSectionStmt = [sql|INSERT INTO section (section) VALUES (?) returning id|]

insertSections :: Connection -> [Section] -> IO [Index]
insertSections conn = returning conn insertSectionStmt

insertArtSectPivotStmt :: Query
insertArtSectPivotStmt =
   [sql|INSERT INTO article_section (article_id,section_id) VALUES (?,?)|]

-- hint: use Pivots module for pivot inserts

instance ToRow Section where
   toRow (Sect sect) = [toField sect]

-- with storeSections defined, define a new storeAncilliary to extract and
-- store values for keywords, authors and sections.

storeAncilliary :: Connection -> [IxValue (DatedArticle Authors)] -> IO ()
storeAncilliary conn arts =
   storeSubjects conn arts >>
   storeAuthors conn arts  >>
   storeSections conn arts

-- and apply this storeAncilliary to the etl function, and you've captured
-- the information we're interested in with the articles sampled.

-- Hint: use pa to process the articles

{--
>>> withConnection (flip (etl pa storeAncilliary) sample)

There are 72 sections:

$ select * from section LIMIT 10;

id	section
1	business/banking
2	business/field-notes
3	business/stocks
4	entertainment/arts
5	entertainment/music
6	entertainment/tv/larry-bonko
7	inside-business/calendar
8	inside-business/news/columns
9	inside-business/news/economic-development
10	inside-business/news/first-person-features

There are 125 article-section pivots

$ select * from article_section LIMIT 10;

id	article_id	section_10
1	1		68
2	2		30
3	3		38
4	4		56
5	5		51
6	6		57
7	7		58
8	8		38
9	9		33
10	9		34
--}

{-- BONUS -----------------------------------------------------------------

Keywords, Authors, and sections. All three follow the same pattern:

* fetch previously stored values
* memoize
* materials values from article set, see which values are new
* store new values, relate all values.

If we are following the same pattern, is there a generalization of the
functions for all of this, so we can have one function that does the work
for any memoize-y type? What would this function look like?

memoizeStore :: ToRow a => Ord a =>
                Connection
             -> (Connection -> IO [IxValue a])
             -> (Connection -> [a] -> IO [Index])
             -> Query
             -> (DatedArticle Authors -> [a])
             -> [IxValue (DatedArticle Authors)]
             -> IO ()
memoizeStore conn fetcher storer pivotQuery getter ixarts =

-- so, 1. we need to fetch currently-stored values and start the MemoizingTable

   fetcher conn >>= \ixvals -> 
   let memtable = MT.start (map ix2tup ixvals)
       (ids,arts) = unzip (map ix2tup ixarts)
       stat = execState (zipWithM_ MT.triageM ids (map getter arts))
                                   (memtable,Map.empty)
       substate = Set.toList (MT.newValues (fst stat))
   in  storer conn substate >>= \ixnewvals ->
   let table = MT.update (zip (map idx ixnewvals) substate) (fst stat)
   in  void (executeMany conn pivotQuery
                         (evalState buildPivots (table, snd stat)))

-- AHA! IT WORKED! Moving this to the Pivots module after this exercise.
--}
