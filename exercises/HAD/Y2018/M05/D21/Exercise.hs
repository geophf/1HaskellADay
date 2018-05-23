{-# LANGUAGE QuasiQuotes #-}

module Y2018.M05.D21.Exercise where

-- Today, we insert NEW articles into the database. Recall from

import Y2018.M05.D17.Exercise (processArts)

-- we triaged articles by NEW, UPDATED, and REDUNDANT, so here is the NEW:

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (intercalate)
import qualified Data.Map as Map 
import Data.Maybe (mapMaybe, maybeToList)                
import Data.Time
import Data.Tuple (swap)                                 
   
import Database.PostgreSQL.Simple                
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
   
import Text.HTML.TagSoup (Tag) 
import qualified Text.HTML.TagSoup as TS

-- below imports available via 1HaskellADay git repository

import Data.MemoizingTable (MemoizingTable(MT), MemoizingS)
import qualified Data.MemoizingTable as MT
import Data.Logger
import Data.LookupTable
import Data.Time.Stamped

import Store.SQL.Connection
import Store.SQL.Util.AuditLogging (storeAuditInfo)
import Store.SQL.Util.Indexed
import Store.SQL.Util.Logging
import Store.SQL.Util.LookupTable (lookupTable)
import Store.SQL.Util.Pivots (joinValue)

import Y2017.M11.D01.Exercise -- for the special character table

import Y2018.M04.D02.Exercise hiding (idx)      -- for Article
import Y2018.M04.D03.Exercise (Author, author)
import Y2018.M04.D04.Exercise   -- storing authors
import Y2018.M04.D09.Exercise hiding (Tag, tagStmt)  -- storing tags
import Y2018.M04.D11.Exercise   -- storing packets
import Y2018.M04.D12.Exercise   -- auditing and logs
import Y2018.M04.D13.Exercise   -- packets, protec-ted
import Y2018.M04.D18.Exercise   -- storing unparsed article JSON
import Y2018.M05.D04.Exercise   -- REST endpoint articles: get
import Y2018.M05.D07.Exercise   -- ArticleMetaData from database
import Y2018.M05.D08.Exercise   -- triaging

-- So: here we go.

type MAuthors m a = MemoizingS m Integer Author a

newArticle :: Connection -> SpecialCharTable -> LookupTable -> Integer
           -> WPJATI -> MAuthors IO [Index]
newArticle conn spc lk pidx art = undefined

-- inserts the JSON of the article and the article itself, properly subdivided
-- as follows:

packStmt :: Query
packStmt = [sql|INSERT INTO article_packet (article_id,packet_id) VALUES (?,?)|]

tagStmt :: Query
tagStmt = [sql|INSERT INTO article_tag (article_id,tag_id) VALUES (?,?)|]

catStmt :: Query
catStmt = [sql|INSERT INTO article_category (article_id,category_id) VALUES (?,?)|]

authStmt :: Query
authStmt = [sql|INSERT INTO article_author (article_id,author_id) VALUES (?,?)|]

-- the above imports may help

{-- BONUS -----------------------------------------------------------------

NEW, UPDATE, and REDUNDANT articles: create an ETL process that downloads
an appropriately-sized set of articles from the REST endpoint then uploads
the triaged articles into the database.

Remember to log information as you store articles then to make an audit log
entry when you complete this upload process.
--}

main' :: [String] -> IO ()
main' args = undefined
