module Y2018.M05.D04.Exercise where

{--
We're going to ease back into ETL-like functionality again. The endgame is this:
We want to create a daily upload process that:

1. Queries the database for the most recent entry into the audit logs (date)
2. Fetches a set of (articles) from the REST endpoint up to a week before (date)
3. Triages (articles) into:
   a. redundant
   b. updated
   c. new
   when compared against what's in the data store.
4. Takes the new articles and
   a. discards them
   b. updates them
   c. inserts them
   based upon their triaged state
5. Logs everything and audits the process for review and the next go-'round.

So, let's break this problem into bite-sized pieces, recollecting what we've
done before, reusing what we can, and repurposing what we can.

The vein of these Haskell problems is to use available code/algorithms, even
when data types don't quite match what we've done before.

So: 1. get the most recent audit log entry and offset that by one week.
--}

import Data.Aeson
import Data.Time

import Database.PostgreSQL.Simple

-- below imports available via 1HaskellADay git repository

import Y2018.M01.D29.Exercise (oneWeekAgo)

-- we can't use:

-- import Y2018.M01.D26.Exercise (ow)

-- because that's based on PILOT data structures, not WPJ ones. We fetch
-- WPJ packets from:

import Y2018.M04.D13.Exercise hiding (packetReader, errOut)

-- Packet is declared in Y2018.M04.D13.Exercise, but we need to convert the
-- Value values to Article values to get the date published

import Y2018.M04.D02.Exercise -- for FromJSON Article

-- Say the the result of oneWeekAgo for WPJ database is (date). Did you have 
-- to change anything to get that result?

-- Okay, that was too easy.

-- 2. Fetch a set of articles from the rest endpoint upto (date)

type ParsedPacket = (Packet Value, [(Value, Article)])

pack2arts :: Packet Value -> [Article]
pack2arts pack = undefined

-- and from there you can convert a packet to a parsed packet

packetReader :: Day -> Tries -> IO [ParsedPacket]
packetReader weekAgo retry = undefined

-- this time packetReader reads all packets up to (weekAgo)

accumPacket :: Packet Value -> [ParsedPacket] -> IO [ParsedPacket]
accumPacket pack accum = undefined

-- the accumulator function on successful read

errOut :: Day -> Tries -> String -> [ParsedPacket] -> IO [ParsedPacket]
errOut weekAgo retries errmsg accum = undefined

-- How many packets did you consume for a week's worth of articles from today?

downloader :: Connection -> IO (Day, [ParsedPacket])
downloader conn = undefined
