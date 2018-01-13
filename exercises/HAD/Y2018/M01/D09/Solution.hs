{-# LANGUAGE QuasiQuotes #-}

module Y2018.M01.D09.Solution where

{--
Okay, we've parsed articles from JSON and we've stored those articles.

Let's start expanding the scope here both in breath and in depth.

Depth first.

What happens when we don't parse an article? Or we can't store one?

We've logged parsing process information to a Logger-type (of the Writer monad),
today, let's record what we've logged into the database with a new log-table.
--}

import Control.Monad (void)

import Data.Aeson
import Data.Functor.Identity (Identity)
import qualified Data.Map as Map

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- below imports available via 1HaskellADay git repository

import Data.Logger

import Store.SQL.Connection (withConnection)
import Store.SQL.Util.Indexed
import Store.SQL.Util.LookupTable

import Y2017.M12.D27.Solution hiding (pa) -- for DatedArticle
import Y2017.M12.D29.Solution hiding (etl) -- for filtering out AP articles
import Y2018.M01.D02.Solution (parseArticles, storeArticles)
import Y2018.M01.D04.Solution (Authors,pa)
import Y2018.M01.D08.Solution 

data Severity = TRACE | DEBUG | INFO | WARN | ERROR | FATAL
   deriving (Eq, Ord, Show)

instance ToField Severity where
   toField = toField . show

data LogEntry = Entry { sev :: Severity,app,mod,msg :: String }
   deriving (Eq, Show)

data LogEntry' = LE' { ent :: LogEntry, lk :: LookupTable }
   deriving Eq

instance Show LogEntry' where
   show (LE' (Entry sev app mod msg) lk) =
      "Entry' { sev :: " ++ show sev ++ ('/':show (lk Map.! show sev))
          ++ concat (zipWith (\ a b -> ", " ++ a ++ " :: \"" ++ b ++ "\"")
              (words "app mod msg") [app, mod, msg]) ++ " }"

instance ToRow LogEntry where
   toRow (Entry _ a m e) = map toField [a,m,e]

instance ToRow LogEntry' where
   toRow (LE' ent lk) = 
      toField (lk Map.! show (sev ent)):toRow ent

insertLogEntryStmt :: Query
insertLogEntryStmt = 
   [sql|INSERT INTO log (severity,app,module,message) VALUES (?,?,?,?)|]

insertLogEntries :: Connection -> LookupTable -> [LogEntry] -> IO ()
insertLogEntries conn lk =
   void . executeMany conn insertLogEntryStmt . map (`LE'` lk)

-- modify the ETL process from Y2018.M01.D02.Solution to spill the log entries
-- to the database (also, the Logger m should be cleared, so you don't keep
-- reentering them.

-- down the road, we will enhance the logger to be application-specific and
-- tailor behavior around severity. You know, like log4h ('log for haskell')

etl :: BlockParser Identity Authors
    -> (Connection -> [IxValue (DatedArticle Authors)] -> IO ())
    -> Connection -> FilePath -> IO ()
etl generator ancilliaryFn conn json =
   parseArticles generator json                      >>= \(arts,logentries) ->
   lookupTable conn "severity_lk"                    >>= \lk ->
   insertLogEntries conn lk (map mkentry logentries) >>
   storeArticles conn arts                           >>= \ixarts ->
   storeAncilliary conn ixarts                       >>
   insertLogEntries conn lk [mkentry ("stored " ++ (show $ length ixarts)
                                                ++ " articles")]
      where mkentry = Entry INFO "etl_pilot" "Y2018.M01.D09.Solution"

-- moving LogEntry etc to Data.Logger
-- moving SQL marshalling to Store.SQL.Util.Logging
