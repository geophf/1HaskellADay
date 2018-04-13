{-# LANGUAGE OverloadedStrings, QuasiQuotes, TupleSections #-}

module Y2018.M01.D31.Solution where

{--
Yesterday we downloaded the appropriate number of packets from the REST 
endpoint, and, comparing the articles in the packets to the metadata of the
articles in the database, we were able to triage the downloaded articles.

Today, we'll take the triaged articles, along with the metadata, and either
upload or update the articles in the database.
--}

import Prelude hiding (log)

import Codec.Text.IConv (convert, EncodingName)

import Control.Arrow (second)
import Control.Monad.State (lift)
import Control.Monad.Writer (runWriterT)

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Functor (void)
import Data.List (intercalate, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Time
import Data.Time.Clock

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

-- below imports available via 1HaskellADay git repository

import Control.DList (dlToList)
import Data.HTML -- (htmlBlock, plainText, demark)
import Data.Logger
import Data.Time.Stamped (StampedWriter, sayIO)
    -- for timestamping things, like logs

import Store.SQL.Connection
import Store.SQL.Util.AuditLogging (storeAuditInfo)
import Store.SQL.Util.Indexed
import Store.SQL.Util.Logging
import Store.SQL.Util.LookupTable

import Y2017.M12.D20.Solution (Packet, Block, next)
import Y2017.M12.D26.Solution (insertStagedArt)
import Y2017.M12.D27.Solution (insertArts, DatedArticle, prologue, lastupdated)
import Y2017.M12.D29.Solution (apArt)

-- import Y2018.M01.D15.Solution (elt) -- hint for etl process

import Y2018.M01.D02.Solution (storeArticles)
import Y2018.M01.D04.Solution (Authors)
-- import Y2018.M01.D10.Solution (insertPackets)
import Y2018.M01.D26.Solution -- for downloading packets from REST endpoint
import Y2018.M01.D29.Solution (oneWeekAgo)
import Y2018.M01.D30.Solution (ArticleTriageInformation(art, ATI), Triage(NEW, UPDATED, REDUNDANT), ArticleMetaData(artId), triageArticles, articles, megapacket, fetchArticleMetaData)

import Y2018.M02.D06.Solution (insertPackets, insertPacketArtPvtStmt, pivotArtPacket)

-- for uploading the articles see the process of the gruntWerk for uploading
-- the staged articles then the articles, themselves.

-- Do that.

-- So, now, let's look at the updating issue. We're NOT going to change anything
-- other than the article table (oops, and the block table, which we can get
-- when we update the article table)

updateArtStmt :: IxValue (DatedArticle a) -> Query
updateArtStmt (IxV idx art) =
   let (p:arams) = mapMaybe sequence [("abstract", demark <$> prologue art),
         ("update_dt", show <$> lastupdated art),
         ("full_text", Just (unlines $ plainText art)),
         ("rendered_text", Just (htmlBlock art))]
       strfn (a,b) = a ++ ('=':enquote b)
       rendered = strfn p ++ concatMap ((", " ++) . strfn) arams
   in Query (B.pack ("UPDATE article SET " ++ rendered
                   ++ " WHERE id=" ++ show idx ++ " returning src_id"))

enquote :: String -> String
enquote str = quote ++ convert' "LATIN1" "UTF8" str ++ quote
   where quote = "$sqs$"

convert' :: EncodingName -> EncodingName -> String -> String
convert' src dest = BL.unpack . convert src dest . BL.pack

-- enquote surrounds a string with SQL quote tokens. As our text has
-- embedded single quotes this: 'user's manual' throws a SQL syntax error.
-- quote escapes the quote-symbol.

updateArts :: Connection -> [IxValue (DatedArticle a)] -> IO [Index]
updateArts conn arts =

   -- it looks like, because of the nature of the return, that we have
   -- to do this as a mapM function. m'kay. This will hurt.

   concat <$> mapM (query' conn . updateArtStmt) arts
      where query' = query_ -- conn stmt = print stmt >> query_ conn stmt

-- now we need to update the source blocks:

updateBlockStmt :: IxValue Block -> String
updateBlockStmt (IxV idx blk) =
   "UPDATE article_stg SET block=" ++ enquote (BL.unpack (encodePretty blk))
        ++ " WHERE id=" ++ show idx

updateBlockStmts :: [IxValue Block] -> Query
updateBlockStmts = Query . B.pack . intercalate "; " . map updateBlockStmt

updateBlocks :: Connection -> [IxValue Block] -> IO ()
updateBlocks conn = void . execute_ conn . updateBlockStmts

   -- same problem? Nope: we can execute_ many statements here

-- so, we need to figure out how to get from the ArticleTriageInformation
-- to a set of indexed articles (that return block indices) then to a set
-- of indexed blocks ... make sure the articles are sorted going in so that
-- the sorted results coming out match the block ids (I learned that one the
-- hard way!

insertAndUpdate :: Connection -> Map Triage [ArticleTriageInformation]
                -> StampedWriter LogEntry [IxValue (DatedArticle Authors)]
insertAndUpdate conn triage =
   concat <$> mapM (uncurry (dbAction conn)) (Map.toList triage)

log :: Severity -> String -> StampedWriter LogEntry ()
log sev = sayIO . Entry sev "daily upload" "Y2018.M01.D31.Solution"

action, kind :: Triage -> String
action NEW = "Sav"
action UPDATED = "Updat"
action REDUNDANT = "Elid"

kind NEW = "new "
kind UPDATED = ""
kind REDUNDANT = "redundant "

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

-- sort articles by IX
-- call updateArts

info2ixarts :: ArticleTriageInformation
            -> Maybe (IxValue (DatedArticle Authors), Block)
info2ixarts (ATI _ (b, a) amd) = (,b) . flip IxV a . artId <$> amd

{-- BONUS -----------------------------------------------------------------

Now, put this all together to create the daily upload process
--}

dailyUpload :: Connection -> Day -> [ArticleMetaData]
            -> StampedWriter LogEntry (Maybe (Packet, IxValue (DatedArticle Authors)))
dailyUpload conn date amd =
   ow date 0 []                                             >>= \packs ->
   let arts' = articles packs 
       len   = length arts' in
   log INFO ("Downloaded " ++ show len ++ " articles.")     >>
   let filt = filter (apArt . snd . snd) arts'
       filtOut = show (length arts' - length filt) in
   log INFO ("Filtered out " ++ filtOut ++ " AP articles.") >>
   let bins = triageArticles amd filt
   in  insertAndUpdate conn bins                            >>= \arts ->

-- there may be no articles to process

       list (return Nothing)
            (\as -> let pack = megapacket bins in
                    lift (insertPackets conn [pack])         >>= \pacIds ->
                    log INFO ("Inserted " ++ show pack)      >>
                    let pivs = concatMap (pivotArtPacket as) pacIds in
                    lift (executeMany conn insertPacketArtPvtStmt pivs) >>
                    return (Just (pack, last as))) arts

list :: b -> ([a] -> b) -> [a] -> b
list ans _ []      = ans
list _ f lst@(_:_) = f lst

-- reminders:
-- * get the oneWeekAgo value
-- * download the packets from the REST endpoint
-- * triage the articles against what's in the database
-- * create the megapacket from the triaged articles
-- * log entries
-- * storeArticles
-- * update articles
-- * audit entry

main' :: [String] -> IO ()
main' [] = errorMsg
main' ["go"] =
   putStrLn "Running daily_update..."                             >>
   getCurrentTime                                                 >>= \start ->
   putStrLn ("Start time: " ++ show start)                        >>
   withConnection PILOT (\conn ->
      oneWeekAgo conn                               >>= \date ->
      fetchArticleMetaData conn (addDays (-1) date) >>= \amd ->
      runWriterT (dailyUpload conn date amd)        >>= \(pack, logs) ->
      lookupTable conn "severity_lk"                >>= \sev ->
      insertStampedEntries conn sev (dlToList logs) >>
      maybe (return ()) 
            (\(p,a) -> let nxt = show (next p) in
                       lookupTable conn "active_lk" >>= \actv ->
                       lookupTable conn "action_lk" >>= \actn ->
                       storeAuditInfo conn actv actn nxt a) pack) >>
      getCurrentTime                                              >>=
      putStrLn . ("Daily update process complete: " ++)
               . show . flip diffUTCTime start
main' _ = errorMsg

errorMsg :: IO ()
errorMsg = putStrLn (unlines ["", "daily_upload <go>", "",
    "\tuploads Pilot articles from the REST endpoint to the database",
    "\t\tenviroment contains database connectivity", ""])

-- some (or all?) of this functionality may be handled for you already
-- by functions we've defined in exercises prior.
