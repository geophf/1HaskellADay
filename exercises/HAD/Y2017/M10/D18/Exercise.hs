module Y2017.M10.D18.Exercise where

{--
Today we are going to take the Grouping value, which, as you recall, is:

type Grouping = Map Topic (Map Day [Title])

(this from module Y2017.M10.D04.Exercise)

And we're going to look at it in a different way than how we did with the circle
packing we did in that module and in Y2017.M10.D09.Exercise.

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
import Data.Time

-- below imports available via 1HaskellADay git repository

import Data.Archive
import Y2017.M10.D04.Exercise
import Y2017.M10.D05.Exercise
import Y2017.M10.D17.Exercise

type Regroup = Archive (Map Topic Int)

regroup :: Grouping -> Regroup
regroup group = undefined

-- regroup regroups the group from a topic-centric bias to a day-centric one
-- and turns the list of articles (in this case per topic per day) to a count

{--
Now, with the minimal, relevant set of topics you created in Y2017.M10.D05,
regroup them then spit them out to a CSV file
--}

regroupTo :: FilePath -> Grouping -> IO ()
regroupTo csvout group = undefined

{-- BONUS -----------------------------------------------------------------

Well, from yesterday's app, we have already computed the grouping, so:
roll this functionality into the charter app you created yesterday.

Run charter against the NYT compressed archives in Y2017/M10/D03/NYT*.gz

What are your results? Upload your charts to twitter.
--}
