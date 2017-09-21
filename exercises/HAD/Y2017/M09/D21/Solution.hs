{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M09.D21.Solution where

import Control.Arrow ((***))
import Control.Monad (void)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow

import Network.HTTP.Conduit

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection (connectInfo)

import Y2017.M09.D14.Solution
import Y2017.M09.D15.Solution

-- and, of course, we'll use the work we did yesterday to insert values into
-- the key store ...

import Y2017.M09.D20.Solution

{-- 
Yesterday, as you see above, we extracted keywords from a file then stored them
on a PostgreSQL data store. Great.

So now we have some populated values in our key-dictionary.

Today, we are going to continue this good work, but to continue work, we need
to know the state of where we left off yesterday, or whenever we left off work.

So, given the Key data type from yesterday, read in the key values from
the data store.  You'll need a FromRow instance for it, now.
--}

instance FromRow Key where
   fromRow = Key <$> field <*> field

-- and a SQL query

readKeysStmt :: Query
readKeysStmt = [sql|SELECT * FROM keyword|]

readKeys :: Connection -> IO (Dictionary, Int)
readKeys conn =
   foldr (\(Key idx key) -> Map.insert key idx *** max idx)
         (Map.empty, 0) <$> query_ conn readKeysStmt

-- and from the set of keys, you can construct a Dictionary, knowing the
-- max index, and reify that to a WordContext.

{-- 
>>> connectInfo 
ConnectInfo {connectHost = "..." ...}
>>> conn <- connect it
>>> (dict, top) <- readKeys conn
>>> length dict
91
>>> top
90

Do you know what that means?

That means you can continue the work on all the other files in articlesDir

But let's just do one more today. Let's say:

select id from article where article_id='AP900327-0242.txt'

returns 3.

Do what you did yesterday, that is: rend the article, extract the keywords,
but this time use the WordContext value you've built from querying the data
store.

But here's the thing, you only want to insert new keys into the keyword table,
not keys that are already there... So you need to retain the stop that you
got from reading the key table in from the database and write the keys after
that stop, only.
--}

insertNewKeys :: Connection -> Int -> WordContext -> IO ()
insertNewKeys conn stop =
   insertAllKeys conn . Map.filterWithKey (const (> stop)) . dict

{-- 
With that, you can insert into the article_keyword table as before, using
the insertAllArticlesKeyWords function (how?)

With AP900327-0242.txt, insert new keys and keywords for artId = 3.

>>> wordcontext <- kea (succ top) dict <$> BL.readFile "Y2017/M09/D08/articles/b/AP900327-0242.txt" 
>>> insertNewKeys conn top wordcontext 

and in the SQL database:

select count(1) from keyword;

returns 331. WOOT!

Then:

>>> insertAllArticleKeyWords conn 3 (kws wordcontext)

select count(1) from article_keyword;

returns 356 ... WOOT! WOOT!

>>> close conn

Here are the top ten keywords across both articles scanned:

select * from article_keyword
order by keyword_strength desc
LIMIT 10;

id	article_id	keyword_id	keyword_strength	keyword_count
-----------------------------------------------------------------------------
92	3		0		0.0940499		49
1	2		0		0.07913669		11
94	3		6		0.044145875		23
10	2		9		0.035971224		5
34	2		33		0.035971224		5
2	2		1		0.02877698		4
38	2		37		0.02877698		4
120	3		94		0.024952015		13
105	3		37		0.024952015		13
125	3		99		0.02303263		12

--}
