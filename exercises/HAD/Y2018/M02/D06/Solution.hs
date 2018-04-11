{-# LANGUAGE QuasiQuotes #-}

module Y2018.M02.D06.Solution where

{--
What were the articles uploaded or updated today? How do we find that out?

We ... can determine this, given our current upload process: we look in the
logs. This, however, is not desireable: we simply want to answer a question
with no manual (parsing) intervention. The logs are good for telling us what
happened, but they aren't good for simple, automatic answers to questions, like
the one above.

So, let's solve this specific problem by introducing a packet pivot table. For
each packet of articles insert or updated, we join the article id with the
packet id. The packet ... 

BUT WAIT! The packet does not have a timestamp of when it was insert.

Thus the scope of today's Haskell exercise grows before our very eyes!

How often that Haskell exercises imitate Life.*

*Life, n.: scope creep, also known as 'project (mis)management.' 
--}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- below imports available via 1HaskellADay git repository

import Data.Time.Stamped (Stamped, stampIt)

import Store.SQL.Util.Indexed (IxValue, Index)
import Store.SQL.Util.Pivots (Pivot, joinValue)

import Y2017.M12.D20.Solution (Packet)
import Y2017.M12.D27.Solution (DatedArticle)

import Y2018.M01.D02.Solution (storeArticles)
import Y2018.M01.D16.Solution (readPacket)
import Y2018.M01.D19.Solution (etl)

-- note-to-self: ALTER TABLE packet ...

insertStampedPacketStmt :: Query
insertStampedPacketStmt =
   [sql|INSERT INTO packet (time,view,prev,next,total,count)
        VALUES (?,?,?,?,?,?)
        returning id|]

-- now for the insert packet statement, is the exact time all that crucial?
-- I don't think so, so we can simply take unstamped packets as the argument
-- and enhance those values with timestamps on insert:

insertPackets :: Connection -> [Packet] -> IO [Index]
insertPackets conn packs =
   mapM stampIt packs >>= returning conn insertStampedPacketStmt

-- we also need the indices to create the pivot between packets and articles.

-- Now, when you store the articles, add the pivot information to the database

-- see storeArticles

-- note-to-self: CREATE TABLE article_packet ...

insertPacketArtPvtStmt :: Query
insertPacketArtPvtStmt =
   [sql|INSERT INTO article_packet (article_id,packet_id) VALUES (?,?)|]

pivotArtPacket :: [IxValue (DatedArticle a)] -> Index -> [Pivot]
pivotArtPacket arts = zipWith joinValue arts . repeat

-- hint: is there a function that does this in module Pivots?

-- with the pivotArtPackage function, you can use the pivot insert function
-- from Store.SQL.Util.Pivots

-- read a packet from the REST endpoint and insert the articles, as you have
-- done before. How many entries were made into the article_packet pivot table?

{--
$ select count(1) from article_packet;
132

woot.
--}
