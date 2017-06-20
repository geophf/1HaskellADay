module Y2017.M06.D21.Exercise where

import Database.Persist

-- below import available via 1HaskellADay git repository

import Y2017.M06.D16.Exercise

{--
Okay, a new direction (but not One Direction, please). If you don't have the
persistent-framework, or one like it, download and install it.

$ cabal update
$ cabal install persistent

Now, with your SQL database of choice (a lot of examples are with SQLite, so you
can use that, if you'd like), create a database then in that a table named
'HoneyDo' with columns 'doId' 'item' and 'status.'

Populate this data table with the following data:
--}

twoRows :: [(Int, String, String)]
twoRows = [(1, "just run like verb", "todo"),
           (2, "go to Vapianos on Fetes des Pères", "todo")]

-- (you can populate this table externally to the Haskell runtime)

{-- 
Now, query the data table and extract those data into Haskell values. Use the
functionality you developed (see import above) for the environmental variables
values to connect to your external database.

something like:
--}

data Todo = HoneyDo { id :: Int, task, status :: String }
   deriving (Eq, Show)

sqlQuery :: IO [Todo]
sqlQuery = undefined

-- Now, add a new row to the data table in your SQL database and query the
-- database again. It should be the case the the old query result has one
-- less row than the new query result. LET'S FIND OUT!

-- (This is so exciting!)
