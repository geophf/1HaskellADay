{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M09.D21.Exercise where

import Control.Monad (void)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow

import Network.HTTP.Conduit

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection (connectInfo)

import Y2017.M09.D14.Exercise
import Y2017.M09.D15.Exercise

-- and, of course, we'll use the work we did yesterday to insert values into
-- the key store ...

import Y2017.M09.D20.Exercise

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
   fromRow = undefined

-- and a SQL query

readKeysStmt :: Query
readKeysStmt = [sql|SELECT * FROM keyword|]

readKeys :: Connection -> IO ([Key], Int)
readKeys conn = undefined

-- and from the set of keys, you can construct a Dictionary, knowing the
-- max index, and reify that to a WordContext.

{-- 
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

insertNewKeys :: WordContext -> Int -> Connection -> IO ()
insertNewKeys ctx stop conn = undefined

-- with that, you can insert into the article_keyword table as before, using
-- the insertAllArticlesKeyWords function (how?)

-- With AP900327-0242.txt, insert new keys and keywords for artId = 3.
--}
