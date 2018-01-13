{-# LANGUAGE QuasiQuotes #-}

module Y2018.M01.D10.Solution where

{--
Today's Haskell exercise is a bit of a warming-up exercise. Today we'll store
packet information in the packet table (the structure of the table matching
Packet (see Y2017.M12.D20.Solution), and also write the fetch function that
retrieves packet information from the database from a given id (which we will
provide later).

We do this for auditing purposes. We wish to store not only the articles that
we store, but also information about what we stored and when. Storing packet
information is a piece of that puzzle.

... hm. Actually, I don't think a fetch function is necessary given how I'm
planning auditing. We shall see. For today, just write the toRow function
--}

import Control.Arrow (second)
import Control.Monad (void)
import Control.Monad.Writer (runWriter)

import Data.Aeson
import Data.Functor.Identity (Identity)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow

-- below import available via 1HaskellADay git repository

import Control.DList (dlToList)

import Data.Logger
import Data.LookupTable (LookupTable)

import Store.SQL.Connection (withConnection)
import Store.SQL.Util.Indexed
import Store.SQL.Util.Logging
import Store.SQL.Util.LookupTable

import Y2017.M12.D20.Solution -- for Packet
import Y2017.M12.D27.Solution hiding (pa) -- for DatedArticle
import Y2017.M12.D29.Solution hiding (etl) -- for BlockParser

import Y2018.M01.D02.Solution hiding (etl, storeAncilliary, parseArticles)
import Y2018.M01.D04.Solution hiding (etl, storeAncilliary)
import Y2018.M01.D08.Solution -- for storeAncilliary

insertPacketStmt :: Query
insertPacketStmt =
   [sql|INSERT INTO packet (view,prev,next,total,count) VALUES (?,?,?,?,?)|]

insertPackets :: Connection -> [Packet] -> IO ()
insertPackets conn = void . executeMany conn insertPacketStmt

instance ToRow Packet where
   toRow (Pack v c t n p _) = [toField v, toField p] ++ map toField [n,t,c]

{--
Using Y2017.M12.D20.Exercise.readSample, load in the packet and store its
information.
--}

{-- BONUS -----------------------------------------------------------------

Rewrite the etl-process to store the packet information (that is: don't discard
packet information anymore) along with the articles. Remember to include logging
functionality as well!
--}

readStorePacket :: Connection -> FilePath -> IO Packet
readStorePacket conn jsonFile =
   readSample jsonFile >>= \pack ->
   insertPackets conn [pack] >>
   return pack

parseArticles :: FromJSON a => BlockParser Identity a -> Packet
              -> ([(Block,Maybe (DatedArticle a))], [String])
parseArticles generator =
   second dlToList . runWriter . elide generator apArt . rows

-- so we want to do all the work of the ETL AND return the indexed articles and
-- packet for down-the-road work, as necessary

gruntWerk ::  BlockParser Identity Authors
    -> (Connection -> [IxValue (DatedArticle Authors)] -> IO ())
    -> Connection -> FilePath -> IO (Packet, IxValue (DatedArticle Authors))
gruntWerk generator ancillaryFn conn jsonFile =
   lookupTable conn "severity_lk"                    >>= \lk ->
   readStorePacket conn jsonFile                     >>= \pack ->
   let (arts,logentries) = parseArticles generator pack in
   insertLogEntries conn lk [mkentry ("Inserted "
                      ++ show (pack { rows = [] }))] >>
   insertLogEntries conn lk (map mkentry logentries) >>
   storeArticles conn arts                           >>= \ixarts ->
   storeAncilliary conn ixarts                       >>
   insertLogEntries conn lk [mkentry ("stored " ++ (show $ length ixarts)
                                    ++ " articles")] >>
   return (pack, last ixarts)
      where mkentry = Entry INFO "etl_pilot" "Y2018.M01.D10.Solution"

etl :: BlockParser Identity Authors
    -> (Connection -> [IxValue (DatedArticle Authors)] -> IO ())
    -> Connection -> FilePath -> IO ()
etl generator ancillaryFn conn = void . gruntWerk generator ancillaryFn conn
