{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Y2018.M01.D02.Exercise where

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

import Data.Aeson
import Database.PostgreSQL.Simple

-- Below imports available via 1HaskellADay git repository

import Store.SQL.Connection (withConnection)
import Store.SQL.Util.Indexed
import Store.SQL.Util.Pivots

import Y2017.M10.D03.Exercise -- for Subject storage and retrieval
import Y2017.M12.D20.Exercise -- for Block
import Y2017.M12.D27.Exercise -- for DatedArticle
import Y2017.M12.D29.Exercise -- for filtering out AP articles

sample :: FilePath
sample = "Y2017/M12/D20/sample.json"

kw2subj :: Value -> Subject
kw2subj kw = undefined

{-- BONUS -----------------------------------------------------------------

Store the articles as per Y2017.M12.D29.Exercise and, add to the ETL process
storing ancilliary information, which in this case is storing the keywords
as subjects.
--}

storeArticles :: Connection -> [Block] -> IO [IxValue (DatedArticle Value)]
storeArticles conn blocks = undefined

storeAncilliary :: Connection -> [IxValue (DatedArticle Value)] -> IO ()
storeAncilliary conn artIds = undefined

etl :: (Connection -> [IxValue (DatedArticle Value)] -> IO ()) -> Connection
    -> FilePath -> IO ()
etl ancilliaryFn conn json = undefined
