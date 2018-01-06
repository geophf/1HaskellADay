{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Y2018.M01.D02.Solution where

{--
OOH! HAPPY NEW YEAR, ALL YE HASKELLERS!

Yesterday we scanned our archive of articles and resolved our DatedArticle
type so that we could, finally, upload all 100 articles to the database.

And even before that, we handled optionality (that we didn't know we had)
gracefully so that we loaded a majority of the articles into the database.

Great!

Now, let's start refining the upload process. We currently do not process:

authors, sections, and keywords.

The level of complexity, as I see it, is as follows (easiest to hardest):

keywords,
authors,
sections.

Let's process keywords today.

Well, here's the thing about keywords: they are Values that are either Strings 
or Numbers, so their show-representation reduces to String, which, in that case
we are now dealing with the Subject-type from before.

So, the trick, today, is to convert a Value to a Subject type and store those
Subject values as we did before.
--}

import Control.Arrow (first, second)
import Control.Monad.State
import Control.Monad.Writer (runWriter)

import Data.Aeson
import Data.Functor.Identity (Identity)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.ToField (ToField)

-- Below imports available via 1HaskellADay git repository

import Control.DList (dlToList)
import qualified Data.MemoizingTable as MT

import Store.SQL.Connection (withConnection)
import Store.SQL.Util.Indexed
import Store.SQL.Util.Pivots

import Y2017.M10.D03.Solution -- for Subject storage and retrieval
import Y2017.M12.D20.Solution -- for Block
import Y2017.M12.D26.Solution -- for inserting staged articles
import Y2017.M12.D27.Solution -- for DatedArticle
import Y2017.M12.D29.Solution hiding (etl) -- for filtering out AP articles

sample :: FilePath
sample = "Y2017/M12/D20/sample.json"

kw2subj :: Value -> Subject
kw2subj = Subj . showVal

{--
>>> blcks <- rows <$> readSample sample
>>> (arts, log) = runWriter (elide apArt blcks)
>>> keywords <$> (snd . head $ tail arts)
Just [String "chesaepake",String "city council",String "panhandling",
      String "beg",String "rules",String "laws",String "ordinance",String "#po6"]
>>> map kw2subj . keywords <$> (snd . head $ tail arts)
Just [Subj {subj = "chesaepake"},Subj {subj = "city council"},
      Subj {subj = "panhandling"},Subj {subj = "beg"},Subj {subj = "rules"},
      Subj {subj = "laws"},Subj {subj = "ordinance"},Subj {subj = "#po6"}]

And we have our subjects. That means we can follow the module to read subjects
from the database and update the subject table.
--}

-- the keywords are already parsed from the articles, so we simply repurpose
-- art2Subj as (map kw2subj . keywords) and rock and roll

instance Subjective (DatedArticle a) where
   subjects = map kw2subj . keywords

{-- BONUS -----------------------------------------------------------------

Store the articles as per Y2017.M12.D29.Exercise and, add to the ETL process
storing ancilliary information, which in this case is storing the keywords
as subjects.
--}

-- parseArticles extracts the blocks, their parsed articles, and the log of
-- the parsing action.

parseArticles :: FromJSON a => BlockParser Identity a -> FilePath
              -> IO ([(Block,Maybe (DatedArticle a))], [String])
parseArticles generator json =
   rows <$> readSample json >>=
   return . second dlToList . runWriter . elide generator apArt

storeArticles :: ToField a => Connection -> [(Block, Maybe (DatedArticle a))]
              -> IO [IxValue (DatedArticle a)]
storeArticles conn blxArts =
   insertStagedArt conn (map fst blxArts) >>= \ixs ->
   let zips = zipWith (\ix (_,mbart) -> sequence (ix,mbart)) ixs blxArts
       ins  = unzip (catMaybes zips) in
   flip (zipWith IxV) (snd ins) <$> (map idx <$> uncurry (insertArts conn) ins)

storeSubjects :: Connection -> [IxValue (DatedArticle a)] -> IO ()
storeSubjects conn ixarts =
   fetchSubjects conn >>= \presubs ->
   let memtable = MT.start (map ix2tup presubs)
       (ids,arts) = unzip (map ix2tup ixarts)
       stat = execState (zipWithM_ MT.triageM ids (map subjects arts))
                                   (memtable,Map.empty)
       substate = fst stat
   in  uploadMT conn substate >>= \ixsubs ->
   let table = MT.update (map ix2tup ixsubs) substate
   in  insertSubjPivot conn (evalState buildSubjectPivots (table, snd stat))

storeAncilliary :: Connection -> [IxValue (DatedArticle a)] -> IO ()
storeAncilliary = storeSubjects -- but expanded laterz

etl :: ToField a => FromJSON a => BlockParser Identity a
    -> (Connection -> [IxValue (DatedArticle a)] -> IO ())
    -> Connection -> FilePath -> IO ()
etl generator ancilliaryFn conn json =
   parseArticles generator json >>=
   storeArticles conn . fst     >>=
   ancilliaryFn conn

{--
>>> withConnection (flip (etl pb storeAncilliary) sample)

$ select count(1) from subject;
453

$ select * from subject;

id	subject
525	candidacy
526	captain hook
527	car
528	careercast
529	casey roberts
530	cash-balance plan
531	cat corner
532	cat toy
533	cbn
534	cellphone
535	chamber of commerce
536	charity

$ select * from article_subject;

id	article_id	subject_id
502	853		454
503	854		539
504	854		557
505	854		758
506	854		508
507	854		802
508	854		687
509	854		753
510	854		467
511	855		820
512	855		715
513	855		582
514	855		746
--}
