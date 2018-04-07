{-# LANGUAGE QuasiQuotes, TupleSections #-}

module Y2018.M02.D07.Solution where

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

import Prelude hiding (log)

import Control.Arrow (second)
import Control.Monad ((>=>))
import Control.Monad.Writer

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import Data.Functor (void)
import Data.List (intercalate, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Time

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types

-- below imports available via 1HaskellADay git repository

import Control.DList (dlToList)

import Data.HTML (demark, plainText, htmlBlock)
import Data.Logger
import Data.Stamped

import Store.SQL.Connection (withConnection, Database(PILOT))
import Store.SQL.Util.AuditLogging
import Store.SQL.Util.Indexed (IxValue(IxV), Index, idx)
import Store.SQL.Util.Logging
import Store.SQL.Util.LookupTable

import Y2017.M12.D20.Solution (Block, Packet, next)
import Y2017.M12.D27.Solution (DatedArticle, prologue, lastupdated)
import Y2017.M12.D29.Solution (apArt)

import Y2018.M01.D02.Solution (storeArticles)
import Y2018.M01.D04.Solution (Authors)
import Y2018.M01.D26.Solution (ow)
import Y2018.M01.D29.Solution (oneWeekAgo)
import Y2018.M01.D30.Solution (Triage, ArticleTriageInformation(ATI), Triage(NEW, UPDATED, REDUNDANT), megapacket, art, artId, ArticleMetaData, articles, triageArticles, fetchArticleMetaData)

import Y2018.M02.D02.Solution (triageSansAP)
import Y2018.M02.D06.Solution (insertPackets, insertPacketArtPvtStmt, pivotArtPacket)

-- So, now, let's look at the updating issue. We're NOT going to change anything
-- other than the article table (oops, and the block table, which we can get
-- when we update the article table)

updateArtStmt :: IxValue (DatedArticle a) -> Query
updateArtStmt (IxV ix art) =
   let (p:arams) = mapMaybe sequence
                       [("abstract", demark <$> prologue art),
                        ("update_dt", show <$> lastupdated art),
                        ("full_text", Just (unlines $ plainText art)),
                        ("rendered_text", Just (htmlBlock art))]
       strfn (a,b) = a ++ ('=':enquote b)
       rendered = strfn p ++ concatMap ((", " ++) . strfn) arams
   in Query (B.pack ("UPDATE article SET " ++ rendered
                   ++ " WHERE id=" ++ show ix ++ " returning src_id"))

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
enquote str = quote ++ convert' "LATIN1" "UTF8" str ++ quote
   where quote = "$sqs$"

-- takes a string converts it from "LATIN1" to "UTF8"
-- (hint: see Codec.Text.IConv.convert) and surrounds it with '$sqs$'

convert' :: EncodingName -> EncodingName -> String -> String
convert' src dest = BL.unpack . convert src dest . BL.pack

-- Okay, so let's write our updater:

updateArts :: Connection -> [IxValue (DatedArticle a)] -> IO [Index]
updateArts conn arts =

   -- it looks like, because of the nature of the return, that we have
   -- to do this as a mapM function. m'kay. This will hurt.

   concat <$> mapM (query' conn . updateArtStmt) arts
      where query' = query_

 -- conn stmt = print stmt >> query_ conn stmt
-- I have this --^ when things blow up and I want to see the raw query.

-- then use the indices returned to update our blocks:

updateBlockStmt :: IxValue Block -> String
updateBlockStmt (IxV idx blk) =
   "UPDATE article_stg SET block=" ++ enquote (BL.unpack (encodePretty blk))
        ++ " WHERE id=" ++ show idx

updateBlockStmts :: [IxValue Block] -> Query
updateBlockStmts = Query . B.pack . intercalate "; " . map updateBlockStmt

-- the above function takes a series of block statements and stitches them
-- together as one SQL query. You can verify for yourself that executing
-- one Query (of multiple statements) is much faster than a mapM over multiple
-- individual statements.

updateBlocks :: Connection -> [IxValue Block] -> IO ()
updateBlocks conn = void . execute_ conn . updateBlockStmts

{-- BONUS -----------------------------------------------------------------

From the triage you did in Y2018.M01.D30.Solution, extract the UPDATED articles,
insert them into the database (along with their associated blocks), then create
and insert the megapacket, joining the updated articles with that packet.

Manage any logged information.

Yeah. All that.
--}

entry :: Severity -> String -> LogEntry
entry sev = Entry sev "daily upload" "Y2018.M02.D07.Solution"

log :: Severity -> String -> StampedWriter LogEntry ()
log sev = sayIO . entry sev

action, kind :: Triage -> String
action NEW = "Sav"
action UPDATED = "Updat"
action REDUNDANT = "Elid"

kind NEW = "new "
kind UPDATED = ""
kind REDUNDANT = "redundant "

processNewArticles :: Connection -> Map Triage [ArticleTriageInformation]
                   -> StampedWriter LogEntry [IxValue (DatedArticle Authors)]
processNewArticles conn triage =
   concat <$> mapM (uncurry (dbAction conn)) (Map.toList triage)

processPackets :: Connection -> Map Triage [ArticleTriageInformation]
               -> [IxValue (DatedArticle Authors)]
               -> StampedWriter LogEntry [IxValue Packet]
processPackets conn triage as =
   lift (pp conn triage as)                            >>= \packs ->
   mapM_ (log INFO . ("Inserted " ++) . show) packs    >>
   return packs

pp :: Connection -> Map Triage [ArticleTriageInformation]
   -> [IxValue (DatedArticle Authors)] -> IO [IxValue Packet]
pp conn triage as =
   let pack = [megapacket triage] in
   insertPackets conn pack                       >>= \packids ->
   let packs = foldMap (pivotArtPacket as) packids in
   executeMany conn insertPacketArtPvtStmt packs >>
   return (zipWith IxV (map idx packids) pack)

processAudit :: Connection -> IxValue Packet -> IO ()
processAudit conn ixv@(IxV _ pack) =
   lookupTable conn "active_lk" >>= \actv ->
   lookupTable conn "action_lk" >>= \actn ->
   storeAuditInfo conn actv actn (show (next pack)) ixv

dbAction :: Connection -> Triage -> [ArticleTriageInformation]
         -> StampedWriter LogEntry [IxValue (DatedArticle Authors)]
dbAction conn tri info =
   log INFO (action tri ++ "ed " ++ show (length info)
         ++ (' ':kind tri ++ "articles")) >>
   lift (dbAction' conn tri info)

dbAction' :: Connection -> Triage -> [ArticleTriageInformation]
          -> IO [IxValue (DatedArticle Authors)]
dbAction' conn NEW n =
   putStrLn ("Inserting " ++ show (length n) ++ " new articles") >>
   storeArticles conn (map (second Just . art) n)
dbAction' conn UPDATED u =
   putStrLn ("Updating " ++ show (length u) ++ " articles") >>
   let (art, blk) = unzip (sortOn (idx . fst) (mapMaybe info2ixarts u)) in
   updateArts conn art                                      >>=
   updateBlocks conn . flip (zipWith IxV) blk . map idx     >>
   return art
dbAction' conn REDUNDANT _ = return []

info2ixarts :: ArticleTriageInformation
            -> Maybe (IxValue (DatedArticle Authors), Block)
info2ixarts (ATI _ (b, a) amd) = (,b) . flip IxV a . artId <$> amd

-- and let's tie it all together:

dailyUpload :: Connection -> Day -> [ArticleMetaData]
            -> StampedWriter LogEntry [IxValue Packet]
dailyUpload conn date amd =
   ow date 0 []                                             >>= \packs ->
   let arts' = articles packs
       len   = length arts' in
   log INFO ("Downloaded " ++ show len ++ " articles.")     >>
   let filt = filter (apArt . snd . snd) arts'
       filtOut = show (length arts' - length filt) in
   log INFO ("Filtered out " ++ filtOut ++ " AP articles.") >>
   let bins = triageArticles amd filt in
   processNewArticles conn bins                             >>=
   processRest conn bins

processRest :: Connection -> Map Triage [ArticleTriageInformation]
            -> [IxValue (DatedArticle Authors)]
            -> StampedWriter LogEntry [IxValue Packet]
processRest conn tri arts@(_:_) =
   processPackets conn tri arts           >>= \packs ->
   lift (mapM_ (processAudit conn) packs) >>
   return packs
processRest _ _ [] = return []

main' :: [String] -> IO ()
main' [] = errorMsg
main' ["go"] =
   putStrLn "Running daily_update..."                            >>
   getCurrentTime                                                >>= \start ->
   putStrLn ("Start time: " ++ show start)                       >>
   withConnection PILOT (\conn ->
      oneWeekAgo conn                               >>= \date ->
      fetchArticleMetaData conn (addDays (-1) date) >>= \amd ->
      runWriterT (dailyUpload conn date amd)        >>= \(packs, logs) ->
      lookupTable conn "severity_lk"                >>= \sev ->
      insertStampedEntries conn sev (dlToList logs) >>
      getCurrentTime                                >>=
      putStrLn . (("Daily update process complete for packets "
                ++ show (map idx packs) ++ " ") ++) . show
                 . flip diffUTCTime start)
main' _ = errorMsg

errorMsg :: IO ()
errorMsg = putStrLn (unlines ["", "daily_upload <go>", "",
    "\tuploads Pilot articles from the REST endpoint to the database",
    "\t\tenviroment contains database connectivity", ""])

{--
>>> main' []

daily_upload <go>

	uploads Pilot articles from the REST endpoint to the database
		enviroment contains database connectivity


>>> main' ["go"]
Running daily_update...
Start time: 2018-02-08 16:56:11.802337 UTC
Inserting 136 new articles
Updating 24 articles
Daily update process complete: 25.405694s
--}

go :: IO ()
go = main' ["go"]
