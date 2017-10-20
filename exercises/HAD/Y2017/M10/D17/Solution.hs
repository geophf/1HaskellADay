{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns, TupleSections #-}

module Y2017.M10.D17.Solution where

{--
We continue to build apps. Today we will take what's in the database and 
generate a report.

Now, here's the thing: the subjects are disjoint from time, but an ETL load
brings in a set of articles for some time-slice. How do we correctly measure
the frequency of subjects in articles?

Put another way: Y2017.M10.D04.Solution took the approach backwards, first
fetching subjects, then their pivots, then the articles associated with the
most-mentioned subjects.

'Most-mentioned' this week may be very different than the previous week, but
neither the subject table nor the article_subject tables have a date column.

But the article table does.

Which leads to today's Haskell problem.

Write an app that queries the database for the week of x and returns the
most popular subjects that is renderable in a spreadsheet or D3 or however
you'd like to render the results.
--}

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow
import System.Environment

-- below imports available via 1HaskellADay git repository

import Control.Logic.Frege ((<<-))

import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.Pivots

import Y2017.M09.D25.Solution     -- Article type
import Y2017.M10.D04.Solution (graphTopics, ArticleSummary, Grouping, Topic, fetchSubjects, Subject)
import Y2017.M10.D05.Solution (visualize)
import Y2017.M10.D09.Solution (top10sTopics, reformGrouping)
import Y2017.M10.D10.Solution (topicCounts)

import Y2017.M10.D19.Solution -- I'M IMPORTING FROM THE FUTURE!

{--
This program runs the analysis against the database and outputs the report

Whoa.

Okay, let's break it down.

First we retrieve our relevant data sets:
--}

articlesByDate :: Connection -> Day -> Day -> IO [ArticleSummary]
articlesByDate conn from to = query conn articlesByDateStmt [from, to]

-- of course, we need us some SQL for that:

articlesByDateStmt :: Query
articlesByDateStmt = [sql|SELECT id,title,publish_dt FROM article
                          WHERE publish_dt >= ? AND publish_dt <= ?|]

-- Now, from those articles, get the associated Pivots...

articleSubjects :: Connection -> [Integer] -> IO [Pivot]
articleSubjects conn = query conn articleSubjectPivotStmt . Only . In

-- again, the SQL:

articleSubjectPivotStmt :: Query
articleSubjectPivotStmt =
   [sql|SELECT article_id,subject_id FROM article_subject
        WHERE article_id IN ?|]

{--
Finally, we get the indexed subjects associated with the articles
(which we get from culling the pivot tables) ...

BUT BEFORE WE GO TO THE DATABASE FOR THE SUBJECT NAMES!

Let's think about this for a moment. Do we need to know the subject names to
know which subjects are relevant?

Answer: no, we don't need that information.

We simply need to know the frequency, first, to get the most frequent mentions.

We can do that computation without subject names, then look up the names after
we do the computation.

...

... on the other hand: loading in the subjects is an operation that takes a
blink of an eye, so ... do that.

... [here is the part that you do that] [hint: check previous exercises this
month]

Now, with the articles, pivots, and subjects we have our universe by which
we can analyze topicality and generate reports in JSON and CSV.

Do that.

type TopicalContext =
   FilePath -> [Subject] -> [Pivot] -> [ArticleSummary] -> IO ()

generateTopicalityCirclesReport :: TopicalContext
generateTopicalityCirclesReport jsonOutFile subjects =
   visualize jsonOutFile . snd <<- top20relevance subjects

generateTopicalityCSV :: TopicalContext
generateTopicalityCSV csvfile subjects =
   topicCounts csvfile . fst <<- top20relevance subjects

Okay, the above is redundant. Let's simplify with:
--}

topNrelevance :: Int -> Set Topic -> [ArticleSummary] -> [Pivot] -> [Subject]
               -> ([(Topic, Int)], Grouping)
topNrelevance n filts articles pivots subjects =
   let grp = graphTopics subjects pivots articles
       tops = top10sTopics grp
       relevant = take n (filter ((`Set.notMember` filts) . fst) tops) in
   (relevant, reformGrouping (Set.fromList (map fst relevant)) grp)

marshalGrouping :: Connection -> Day -> Day -> Int -> IO ([(Topic, Int)], Grouping)
marshalGrouping conn from to n =
   articlesByDate conn from to >>= \arts ->
   parseChartIgnore            >>= \ignores ->
   topNrelevance n ignores arts <$> articleSubjects conn (map idx arts)
                                <*> fetchSubjects conn

-- so now we can use the report generators we've imported

{-- And with the above, we can define our program:

main' :: [String] -> IO ()

main' [reportDir, from, to, n] =
   connectInfo >>= connect >>=                             \conn ->
   marshalGrouping conn (read from) (read to) (read n) >>= \info ->
   close conn                                          >>
   uncurry runReports info reportDir "circles.json" "topics.csv"

main' _ = putStrLn (unlines ["", "charter <reportDir> <fromDay> <toDay> <n>", "",
  "\tAnalyzes database partitioned by <fromDay> to <toDay>, inclusive.",
  "\t\t(connection information in environment).",
  "\tOutputs top <n> circles.json and topics.csv to <reportDir>"])
--}
   
runReports :: [(Topic, Int)] -> Grouping -> FilePath -> FilePath -> FilePath -> IO ()
runReports tops grp reportDir jsonfile csvfile =
   visualize   (outfile jsonfile) grp  >>
   topicCounts (outfile csvfile)  tops >>
   putStrLn ("Analysis complete. Reports written to " ++ reportDir)
      where outfile = (reportDir ++) . ('/':)
