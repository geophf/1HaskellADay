{-# LANGUAGE OverloadedStrings #-}

module Y2017.M10.D10.Solution where

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

import Control.Arrow (second)
import Data.Aeson                    -- if you're using JSON
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromJust)         -- if you're using JSON
import Database.PostgreSQL.Simple    -- if you're connecting to the database

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection (connectInfo)
                                     -- if you're connecting to the database
import Store.SQL.Util.Indexed
import Store.SQL.Util.Pivots

import Y2017.M10.D04.Solution
import Y2017.M10.D05.Solution
import Y2017.M10.D09.Solution (top10sTopics)

topicCounts :: FilePath -> [(Topic, Int)] -> IO ()
topicCounts csvfile =
   writeFile csvfile . unlines . ("Topic,Articles":)
            . map (uncurry (++) . second ((',':) . show))

-- Make it 'look good.' Have a header, format the text, make it so that even 
-- your vice president can pick up the phone and say 'Hello, Federal!'

-- Some of you are too young to have seen that commercial.

{--
>>> subjs <- (rows . fromJust . decode <$> BL.readFile "Y2017/M10/D05/subj.json") :: IO [Subject]
>>> arts <- (rows . fromJust . decode <$> BL.readFile "Y2017/M10/D05/art.json") :: IO [ArticleSummary]
>>> pivs <- (rows . fromJust . decode <$> BL.readFile "Y2017/M10/D05/art-subj.json") :: IO [Pivot]
>>> let grp = graphTopics subjs pivs arts 
>>> let top10s = top10sTopics grp
>>> topicCounts "Y2017/M10/D10/topics.csv" top10s

And in the file you find:

Topic,Articles
Social networks,42
Presidents,30
Hurricanes,25
Books,19
Floods,16
Aliens,15
Immigration policy,15
Motion pictures,15
Theater,14
White supremacists,14
Bills,13
Storm damage,13
Students,13
Families & family life,12
Tournaments & championships,12
Deportation,11
Politics,11
Tennis,11
Art galleries & museums,10
Criminal investigations,10
Political parties,10
--}
