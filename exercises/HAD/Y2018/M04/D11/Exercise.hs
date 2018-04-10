{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2018.M04.D11.Exercise where

{-- 
Like we've done before, we need to download packets of information (articles)
and (eventually) store those articles, but also store the packet information
that we used to download the articles

see also: Y2017.M12.D20.Exercise.

But this is different: the exercise from last year, the packet contained
information about the packet, itself. This REST endpoint has no such packet
information, so, we'll just store what we know about this packet: the count,
the start and end article IDs and the time dowloaded.

... but we don't have to store the article ids, as those are all derived
from the associated join table.

We'll look at downloading a packet of articles in another exercise (spoiler:
Y2018.M04.D12.Exercise), for today, given the below structures, upload the
packet information to the PostgreSQL database.
--}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToRow

-- below imports available via 1HaskellADay git repository

import Data.Time.Stamped

import Store.SQL.Connection
import Store.SQL.Util.Indexed

import Y2018.M04.D02.Exercise (Article)

type PageNumber = Int

type Count = Int

data Protec = Pro { page :: PageNumber, count :: Count, arts :: [Article] }
   deriving Show

-- (I call it Protec for 'reasons' ... yes, I'm weird)

instance ToRow Protec where
   toRow p = undefined

protecStmt :: Query
protecStmt = [sql|INSERT INTO package (time, page, count)
                  VALUES (?, ?, ?) returning id|]

insertProtec :: Connection -> Protec -> IO Index
insertProtec conn prot = undefined

-- Note, we want to insert a Stamped Protec at the time of insert, hint-hint

protec :: Protec
protec = Pro 1 100 []

-- insert the above value. What value do you get in return?

-- we'll do article insertion from Protec values and article-packet join later
