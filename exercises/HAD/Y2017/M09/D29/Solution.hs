{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}

module Y2017.M09.D29.Solution where

{--
Continuing in the name-theme and in the ETL-theme, let's combine the two!

You see from the results yesterday that the unprocessed names are stored
in the database in the article table. The persons are stored in a person table
which is joined to the article table via article_person allowing for any number of
people to be associated with an article. We're going to parse the names we store
in the article table and populate the person table and the join table today.

SO! ETL PROCESS!

Today, we're going to read the raw names, with their indices, from the article table
in the data store, parse those raw names (as we did in Y2017.M09.D27.Exercise),
then store those names in a new data table with the referenced raw name index.

YAY!

(That's one way to go about it, another way is to store the article, parse the
names from the raw text then store the names at the same time.)
--}

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection (connectInfo)

import Y2017.M09.D20.Solution (inserter)
import Y2017.M09.D26.Solution
import Y2017.M09.D27.Solution

-- First off, we need to read the raw names with an index:

instance FromRow RawNames where 
   fromRow = Raw <$> field <*> field

-- using the following query:

selectRawNamesStmt :: Query
selectRawNamesStmt = [sql|SELECT id, people FROM article WHERE people IS NOT NULL|]

-- n.b.: You see what we're doing here? There's no concept of RawNames now in the
-- database entity-relation model (the ER-model), just articles with a people field
-- But we're extracting the RawNames values from the article table. ETL lolneatness.

selectRawNames :: Connection -> IO [RawNames]
selectRawNames = (`query_` selectRawNamesStmt)

{--
>>> connectInfo 
ConnectInfo {connectHost = "..."...}
>>> conn <- connect it
>>> rns <- selectRawNames conn
>>> length rns
9

Side note: thanks to Nathan Lander @NathanLander for resolving the difference
between SELECT (id,people) and SELECT id,people.
--}

-- Okay, now that we've got the raw names, convert them into a meta-person
-- value with the raw-name id and then the parsed person value:

data IxPerson = IPers { articleIdx :: Integer, pers :: Person }
   deriving (Eq, Ord, Show)

raw2persons :: RawNames -> [IxPerson]
raw2persons (Raw idx peeps) = map (IPers idx) (parsePerson peeps)

{--
>>> peeps = concatMap raw2persons rns
>>> length peeps
10

>>> head peeps
IPers {articleIdx = 1, pers = Name {source = "Cuomo, Mario M",
        family = Just "Cuomo", given = Just "Mario", rest = Just "M"}}
--}

-- now with this insert statement:

insertPersStmt :: Query
insertPersStmt = [sql|INSERT INTO person (source_txt,family_name,given_name,rest)
                      VALUES (?,?,?,?) returning id|]

-- construct a ToRow instance of IxPerson

instance ToRow IxPerson where
   toRow (pers -> p) = toField (source p):map toField ([family,given,rest] <*> [p])

-- and then insert the peoples into the database...

data Index = Idx { idx :: Integer } deriving (Eq, Ord, Show)

instance FromRow Index where fromRow = Idx <$> field

insertPers :: Connection -> [IxPerson] -> IO [Index] -- getting the person indices
insertPers conn = returning conn insertPersStmt

{--
>>> ixpeeps <- insertPers conn peeps
>>> ixpeeps
[Idx {idx = 1},Idx {idx = 2},Idx {idx = 3},Idx {idx = 4},Idx {idx = 5},
 Idx {idx = 6},Idx {idx = 7},Idx {idx = 8},Idx {idx = 9},Idx {idx = 10}]

$ select * from person;
id	source_txt		family_name	given_name	rest
----------------------------------------------------------------------
1	Cuomo, Mario M		Cuomo		Mario		M
2	Reagan, Ronald Wilson	Reagan		Ronald		Wilson
3	Cameron, David		Cameron		David		null
4	Obama, Barack		Obama		Barack		null
5	Armstrong, Karen	Armstrong	Karen		null
6	Cuomo, Mario M		Cuomo		Mario		M
7	Rivlin, Reuven		Rivlin		Reuven		null
8	Francis (Pope)		null		Francis		(Pope)
9	Yingluck Shinawatra	Shinawatra	Yingluck	null
10	Baraka, Amiri		Baraka		Amiri		null
--}

-- Now we need to connect the person to the article in the join table:

data ArticlePersonJoin = APJ { articleId, personId :: Integer }
   deriving (Eq, Ord, Show)

joinArticlePersonIds :: IxPerson -> Index -> ArticlePersonJoin
joinArticlePersonIds (articleIdx -> artId) (idx -> persId) = APJ artId persId

instance ToRow ArticlePersonJoin where
   toRow (APJ aid pid) = map toField [aid,pid]

insertArtPersJoinStmt :: Query
insertArtPersJoinStmt =
   [sql|INSERT INTO article_person (article_id,person_id) VALUES (?,?)|]

insertArtPersJoin :: Connection -> [ArticlePersonJoin] -> IO ()
insertArtPersJoin conn = inserter conn insertArtPersJoinStmt

{--
>>> insertArtPersJoin conn (zipWith joinArticlePersonIds peeps ixpeeps)

$ select * from article_person;

id	article_id	person_id
-----------------------------------
1		1	1
2		2	2
3		3	3
4		3	4
5		4	5
6		5	6
7		7	7
8		8	8
9		10	9
10		11	10

How many people did you insert for the article in the database? 10
How many joins were there? 10

and have much rejoicing! YAY! *throws confetti

Now there's the question of how to ensure we only insert a name once, but then
that gets into name-matching algorithms, and I'm going to punt on that for now.
--}
