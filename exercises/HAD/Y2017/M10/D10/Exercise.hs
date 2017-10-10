{-# LANGUAGE OverloadedStrings #-}

module Y2017.M10.D10.Exercise where

{--
ANOTHER CUSTOMER REQUEST!

Okay, the customer can see the data and is very much impressed.

Good job, everyone! Cigars all around!

But now, they want a tabulated list of the hot topics, reduced to just the
topic and the number of articles in that topic for that week.

SO! Do that. From the results you obtained yesterday, instead of outputing it
to D3 or to a scattergram, output the topic name and the number of articles
in that topic for that week. As before, you must grab the data from the database
or from JSON, graph the topics, then make the topics ... topical (to avoid 
information overload).

Output in a format that works for you: to stdout or to CSV, ... whatever you
would like.
--}

import Data.Aeson                    -- if you're using JSON
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromJust)         -- if you're using JSON
import Database.PostgreSQL.Simple    -- if you're connecting to the database

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection (connectInfo)
                                     -- if you're connecting to the database
import Store.SQL.Util.Indexed
import Store.SQL.Util.Pivots

import Y2017.M10.D04.Exercise
import Y2017.M10.D05.Exercise
import Y2017.M10.D09.Exercise (top10sTopics)

topicCounts :: [(Topic, Int)] -> IO ()
topicCounts topics = undefined

-- Make it 'look good.' Have a header, format the text, make it so that even 
-- your vice president can pick up the phone and say 'Hello, Federal!'

-- Some of you are too young to have seen that commercial.
