{-# LANGUAGE QuasiQuotes #-}

module Y2018.M02.D07.Exercise where

{--
Okay, from last week we have a 'megapacket' of articles triaged into which new
articles to insert and which articles need to be updated.

Let's look at the problem of updating the already-present articles today.
--}

import Codec.Text.IConv (convert, EncodingName)

{--
Ooh! Why that import? Well, I found out the hard way that the articles I'm
downloading from my media source are LATIN1, not UTF8, so when I went to 
update those articles in the database with my handy update statement, all SQL-
hell broke loose.

We'll look at that problem today, as well.
--}

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

-- below imports available via 1HaskellADay git repository

import Store.SQL.Util.Indexed (IxValue(IxV), Index)
import Store.SQL.Util.Logging

import Y2017.M12.D20.Exercise (Block)
import Y2017.M12.D27.Exercise (DatedArticle)

import Y2018.M01.D30.Exercise (Triage, ArticleTriageInformation)
import Y2018.M02.D02.Exercise (triageSansAP)
import Y2018.M02.D06.Exercise (insertPackets)

-- So, now, let's look at the updating issue. We're NOT going to change anything
-- other than the article table (oops, and the block table, which we can get
-- when we update the article table)

updateArtStmt :: IxValue (DatedArticle a) -> Query
updateArtStmt art = undefined

{--
Why undefined? Because this is a problem: we have an article ID from the
database (when we triaged the article information) and the updated
information stitched together as an indexed article, the problem becomes
that we want to update only these fields:

abstract, update_dt, full_text, rendered_text for the given article index

updateArtStmt will have the below schema:

UPDATE article
SET abstract=$sqs$[UTF-8 CODEC of abstract]$sqs$,
    update_dt='<update-date>',
    full_text=$sqs$[UTF-8 CODED of full text]$sqs$,
    rendered_text=$sqs$[UTF-8 CODED of rendered text]$sqs$
WHERE id=<article-index>
returning src_id

Why are returning src_id? So we can update the original unparsed block (that's
coming shortly).

Why the symbols '$sqs$' surrounding text?

If you have single-quotes in text you're inserting, it terminates the string
in the SQL statement. That's not good, so we escape the delimiters in the SQL
statement by using PostgreSQL's $$ delimiter and also prevent the token '$$'
to trip us up by inserting characters between the '$' delimiters.

Very complicated. Real-world data is so very complicated.

So, we need a LATIN1 to UTF8 and $sqs$-delimiting function for strings:
--}

enquote :: String -> String
enquote str = undefined
   where quote = "$sqs$"

-- takes a string converts it from "LATIN1" to "UTF8"
-- (hint: see Codec.Text.IConv.convert) and surrounds it with '$sqs$'

-- Okay, so let's write our updater:

updateArts :: Connection -> [IxValue (DatedArticle a)] -> IO [Index]
updateArts conn arts = undefined

-- then use the indices returned to update our blocks:

updateBlockStmt :: IxValue Block -> String
updateBlockStmt (IxV idx blk) =
   "UPDATE article_stg SET block=" ++ enquote (BL.unpack (encodePretty blk))
        ++ " WHERE id=" ++ show idx

updateBlockStmts :: [IxValue Block] -> Query
updateBlockStmts blocks = undefined

-- the above function takes a series of block statements and stitches them
-- together as one SQL query. You can verify for yourself that executing
-- one Query (of multiple statements) is much faster than a mapM over multiple
-- individual statements.

updateBlocks :: Connection -> [IxValue Block] -> IO ()
updateBlocks conn = undefined

{-- BONUS -----------------------------------------------------------------

From the triage you did in Y2018.M01.D30.Exercise, extract the UPDATED articles,
insert them into the database (along with their associated blocks), then create
and insert the megapacket, joining the updated articles with that packet.

Manage any logged information.

Yeah. All that.
--}

processUpdates :: Connection -> Map Triage [ArticleTriageInformation] -> IO ()
processUpdates conn triage = undefined
