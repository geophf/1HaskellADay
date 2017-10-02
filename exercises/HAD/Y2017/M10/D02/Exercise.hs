{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M10.D02.Exercise where

{--
NEW FEATURE REQUEST!

So, we have a database.

Guess what we also have: USERS! WITH USER-REQUESTS! OH, NOES!

So this user request is this: articles are stored in a compressed archive, then
parsed into data (the text of the article) and meta data. The user wants the
unparsed value as a block.

Okay, then!

You've done this before, now, start from a clean database, extract the articles
from the compressed archive, store each unparsed article, then, with a proxy
for the parsed article, store the parsed article with the associated source
article id.

REDO ALERT!

We are going to be revisiting how we do things with this archive because why?
Several reasons:

1. The database assigns indices to these values, so I don't have to.
2. Separating article before was wrong: some articles are separated by an
   underscored delimitor (of 5 characters). We need a better separation function
--}

import qualified Codec.Compression.GZip as GZ
import Control.Monad
import Data.ByteString.Lazy.Char8 (ByteString)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.Inserts (inserter)

import Y2017.M09.D22.Exercise (arts, dir)
import Y2017.M09.D25.Exercise
import Y2017.M09.D26.Exercise (RawNames)
import Y2017.M09.D28.Exercise
import Y2017.M09.D29.Exercise (raw2persons, insertPers, insertArtPersJoinStmt)

-- Block retrieval and storage -----------------------------------------------

data Block = Block { block :: ByteString }
   deriving (Eq, Show)

type Compressed = ByteString

-- first we extract the articles-as-blocks-of-text

extractBlocks :: Compressed -> [Block]
extractBlocks archive = undefined

-- then we store those in a staging table

insertBlockStmt :: Query
insertBlockStmt = [sql|INSERT INTO article_stg (block) VALUES (?) returning id|]

instance ToRow Block where
   toRow blk = undefined

insertBlocks :: Connection -> [Block] -> IO [Index]
insertBlocks conn blks = undefined

-- Now we parse the block into an Article value adding the source id
-- hint look at parseArticle using the Index as the ... well, source block index

block2Article :: MonadPlus m => Index -> Block -> m Article
block2Article idx blk = undefined

-- next, we insert the articles as before, this time getting the indices for
-- the article rows as we insert there.

-- hint: see Y2017.M09.D28.Exercise.insertArts

-- We have the article ids, we have the rows of people already in hand,
-- store the parsed names, then join those parsed names to their source
-- articles.

-- This means RawNames is derived from the returned indices and articles

art2RawNames :: MonadPlus m => Index -> Article -> m RawNames
art2RawNames artId art = undefined

-- and with the RawNames, raw2pers and insertPers you can insert the persons

-- Pivot tables ---------------------------------------------------------------

{--
Last week you were able to scan the database, extract rows of name(s), parse
the names, then store them as parsed entities in the database connected to the
source article via a join-table. These join tables, also known as 'pivot 
tables,' are prevalent and follow a certain structure. Instead of the specific
ArticlePersonJoin structure, we'll declare and use the general Pivot type here.
--}

data Pivot = Pvt { artIx, valId :: Integer }
   deriving (Eq, Ord, Show)

joinValue :: Indexed i => i -> Index -> Pivot
joinValue i j = undefined

-- and now we need a pivot-inserter

instance ToRow Pivot where
   toRow pvt = undefined

-- and to insert the pivots is simply using the pivot table insert statement,
-- (in this case insertArtPersJoinStmt, with the inserter function.

-- Do that. How many name-pivots did you insert?

-- The Pivot table functionality will be moved to Store.SQL.Util.Pivot

{-- BONUS -----------------------------------------------------------------

Create an ETL process that automates all the steps here, from article extract
from the compressed archive to storing all the above in the PostgreSQL database
--}

etlProcess :: FilePath -> Connection -> IO ()
etlProcess archive conn = undefined
