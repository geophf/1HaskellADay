{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M10.D02.Solution where

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
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

-- below imports available via 1HaskellADay git repository

import Control.DList
import Control.Logic.Frege ((-|))
import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.Inserts (byteStr,inserter)
import Store.SQL.Util.Pivots

import Y2017.M09.D22.Solution (arts, dir)
import Y2017.M09.D25.Solution
import Y2017.M09.D26.Solution (RawNames(Raw))
import Y2017.M09.D28.Solution
import Y2017.M09.D29.Solution (raw2persons, insertPers, insertArtPersJoinStmt)

-- Block retrieval and storage -----------------------------------------------

data Block = Block { block :: ByteString }
   deriving (Eq, Show)

type Compressed = ByteString

-- first we extract the articles-as-blocks-of-text

extractBlocks :: Compressed -> [Block]
extractBlocks = eachBlock emptyDL . tail . BL.lines . GZ.decompress

eachBlock :: DList ByteString -> [ByteString] -> [Block]
eachBlock dl [] = reifyBlock dl
eachBlock dl (h:t) = cont (isLineBreak h) h dl t

cont :: Bool -> ByteString -> DList ByteString -> [ByteString] -> [Block]
cont True _ dl t = reifyBlock dl ++ eachBlock emptyDL t
cont False h dl t = eachBlock (dl <| h) t

isLineBreak :: ByteString -> Bool
isLineBreak =
     (== "_______________________________________________________")
   . BL.take 55

reifyBlock :: MonadPlus m => DList ByteString -> m Block
reifyBlock dl = let ans = dlToList dl in
   guard (not (null ans)) *> pure (Block $ BL.unlines ans)

{--
>>> blocks <- extractBlocks <$> BL.readFile (dir ++ arts)
>>> length blocks
11
--}
-- then we store those in a staging table

insertBlockStmt :: Query
insertBlockStmt = [sql|INSERT INTO article_stg (block) VALUES (?) returning id|]

instance ToRow Block where
   toRow (Block blk) = [toField blk]

insertBlocks :: Connection -> [Block] -> IO [Index]
insertBlocks = insertRows insertBlockStmt

{--
>>> connectInfo 
ConnectInfo {connectHost = "...", ...}
>>> conn <- connect it
>>> ixblks <- insertBlocks conn blocks

$ select * from article_stg;

11	Document 11 of 1000 Poetic Voice Wrapped Tight in Its Shifting ...
--}

-- Now we parse the block into an Article value adding the source id
-- hint look at parseArticle using the Index as the ... well, source block index

block2Article :: MonadPlus m => Index -> Block -> m Article
block2Article i blk = parseArticle (idx i) (block blk)

{--
>>> let articles = join (zipWith block2Article ixblks blocks)
>>> length articles
11
>>> srcId (head articles)
1
--}

-- next, we insert the articles as before, this time getting the indices for
-- the article rows as we insert there.

-- hint: see Y2017.M09.D28.Exercise.insertArts

{--
>>> ixarts <- insertArts conn articles
--}

-- We have the article ids, we have the rows of people already in hand,
-- store the parsed names, then join those parsed names to their source
-- articles.

-- This means RawNames is derived from the returned indices and articles

art2RawNames :: Index -> Article -> Maybe RawNames
art2RawNames artId art = Raw (idx artId) <$> Map.lookup "People" (metadata art)

-- and with the RawNames, raw2pers and insertPers you can insert the persons

{--
>>> let rns = catMaybes (zipWith art2RawNames ixarts articles)
>>> rns
[Raw {fromArticle = 1, text = "Cuomo, Mario M"},
 Raw {fromArticle = 2, text = "Reagan, Ronald Wilson"},
 Raw {fromArticle = 3, text = "Obama, Barack Cameron, David"},
 Raw {fromArticle = 4, text = "Armstrong, Karen"},
 Raw {fromArticle = 5, text = "Cuomo, Mario M"},
 Raw {fromArticle = 7, text = "Rivlin, Reuven"},
 Raw {fromArticle = 8, text = "Francis (Pope)"},
 Raw {fromArticle = 10, text = "Yingluck Shinawatra"},
 Raw {fromArticle = 11, text = "Baraka, Amiri"}]

>>> let pers = concatMap raw2persons rns
>>> ixpers <- insertPers conn pers
>>> length ixpers 
10
--}

-- Pivot tables ---------------------------------------------------------------

{--
Last week you were able to scan the database, extract rows of name(s), parse
the names, then store them as parsed entities in the database connected to the
source article via a join-table. These join tables, also known as 'pivot 
tables,' are prevalent and follow a certain structure. Instead of the specific
ArticlePersonJoin structure, we'll declare and use the general Pivot type here.

data Pivot = Pvt { srcIx, trgId :: Integer }
   deriving (Eq, Ord, Show)

joinValue :: Indexed i => i -> Index -> Pivot
joinValue i j = Pvt (idx i) (idx j)

-- and now we need a pivot-inserter

instance ToRow Pivot where
   toRow (Pvt i j) = map toField [i,j]

-- and to insert the pivots is simply using the pivot table insert statement,
-- in this case insertArtPersJoinStmt, with the inserter function.

-- Do that. How many name-pivots did you insert?

>>> inserter conn insertArtPersJoinStmt (zipWith joinValue pers ixpers)
--}

-- The Pivot table functionality will be moved to Store.SQL.Util.Pivots

{-- BONUS -----------------------------------------------------------------

Create an ETL process that automates all the steps here, from article extract
from the compressed archive to storing all the above in the PostgreSQL database
--}

etlProcess :: FilePath -> Connection -> IO ()
etlProcess archive conn = do
   blocks <- extractBlocks <$> BL.readFile archive
   ixblks <- insertBlocks conn blocks
   let articles = join (zipWith block2Article ixblks blocks)
   ixarts <- insertArts conn articles
   let rns = catMaybes (zipWith art2RawNames ixarts articles)
   let pers = concatMap raw2persons rns
   ixpers <- insertPers conn pers
   inserter conn insertArtPersJoinStmt (zipWith joinValue pers ixpers)

{--
>>> connectInfo 
ConnectInfo {connectHost = "...", ...}
>>> conn <- connect it
>>> etlProcess (dir ++ arts) conn
>>> close conn

$ select count(1) from article;
11

Sweet!
--}
