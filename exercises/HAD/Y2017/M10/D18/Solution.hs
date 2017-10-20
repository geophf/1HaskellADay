{-# LANGUAGE ViewPatterns #-}

module Y2017.M10.D18.Solution where

{--
Today we are going to take the Grouping value, which, as you recall, is:

type Grouping = Map Topic (Map Day [Title])

(this from module Y2017.M10.D04.Solution)

And we're going to look at it in a different way than how we did with the circle
packing we did in that module and in Y2017.M10.D09.Solution.

Today we will output our Grouping value (once we've constructed it) as CSV file
with the following format:

Day,<topic1>,<topic2>,...,<topicn>

where <topici> is the name of the ith topic.

Then, each row of data with be that day (the first column), then each subsequent
column will be the counts of the articles for that topic.

Why? Well, spreadsheets, of course. Management loves their spreadsheets.

And, also, we have other data visualizations available to us. Two come to mind
for me: the stacked bar chart or the grouped bar chart. Both are available on
D3js.org.

Let's get to work.
--}

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Time
import Database.PostgreSQL.Simple (connect, close)

-- below imports available via 1HaskellADay git repository

import Control.List (weave)
import Data.Archive
import Store.SQL.Connection (connectInfo)

import Y2017.M10.D04.Solution
import Y2017.M10.D05.Solution
import Y2017.M10.D17.Solution

type Regroup = Archive (Map Topic Int)

regroup :: Grouping -> Regroup
regroup = foldr populateMap Map.empty . Map.toList

-- structure is: [(topic1, mapping day -> [arts]), ...]

-- regroup regroups the group from a topic-centric bias to a day-centric one
-- and turns the list of articles (in this case per topic per day) to a count

populateMap :: (Topic, Map Day [a]) -> Regroup -> Regroup
populateMap (top, Map.toList -> days) = flip (foldr (appendDay top)) days

appendDay :: Topic -> (Day, [a]) -> Regroup -> Regroup
appendDay top (day, length -> cnt) regrp =

-- Let's think about this for a second.

-- The topic for the day is unique.

-- so: there's no 'append' per se; it's an insert ... correct?

   Map.insert day (Map.insert top cnt topmap) regrp
      where topmap = fromMaybe Map.empty (Map.lookup day regrp)

{--
Now, with the minimal, relevant set of topics you created in Y2017.M10.D05,
regroup them then spit them out to a CSV file
--}

regroupTo :: FilePath -> Grouping -> IO ()
regroupTo csvout group =
   let allkeys = Map.keys group
       columns = "Day":allkeys
       grp     = regroup group
   in  writeFile csvout (unlines (weave columns:map (`weaveWith` allkeys)
                                                    (Map.toList grp)))

weaveWith :: (Day, Map Topic Int) -> [Topic] -> String
weaveWith (day, topics) =
   weave . (show day:) . map (maybe "0" show . flip Map.lookup topics)

{-- BONUS -----------------------------------------------------------------

Well, from yesterday's app, we have already computed the grouping, so:
roll this functionality into the charter app you created yesterday.

Run charter against the NYT compressed archives in Y2017/M10/D03/NYT*.gz

What are your results? Upload your charts to twitter.
--}

main' :: [String] -> IO ()

main' [reportDir, from, to, n] =
   connectInfo >>= connect >>=                                    \conn ->
   marshalGrouping conn (read from) (read to) (read n) >>= \(tops, grp) ->
   close conn                                          >>
   regroupTo (reportDir ++ "/stacks.csv") grp          >>
   runReports tops grp reportDir "circles.json" "topics.csv"

main' _ = putStrLn (unlines ["", "charter <reportDir> <fromDay> <toDay> <n>", "",
  "\tAnalyzes database partitioned by <fromDay> to <toDay>, inclusive.",
  "\t\t(connection information in environment;",
  "\t\t ignore subjects in ~/.chartignore).",
  "\tOutputs top <n> circles.json, stacks.csv and topics.csv to <reportDir>"])
