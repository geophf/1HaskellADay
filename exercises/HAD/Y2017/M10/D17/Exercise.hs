{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns, TupleSections #-}

module Y2017.M10.D17.Exercise where

{--
We continue to build apps. Today we will take what's in the database and 
generate a report.

Now, here's the thing: the subjects are disjoint from time, but an ETL load
brings in a set of articles for some time-slice. How do we correctly measure
the frequency of subjects in articles?

Put another way: Y2017.M10.D04.Exercise took the approach backwards, first
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
import Data.Time
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow
import System.Environment

-- below imports available via 1HaskellADay git repository

import Store.SQL.Connection
import Store.SQL.Util.Indexed
import Store.SQL.Util.Pivots

import Y2017.M09.D25.Exercise     -- Article type
import Y2017.M10.D04.Exercise (graphTopics, ArticleSummary, Subject)
import Y2017.M10.D09.Exercise (top10sTopics)
import Y2017.M10.D10.Exercise (topicCounts)

main :: IO ()
main = undefined

{--
main runs the analysis against the database and outputs the report

Whoa.

Okay, let's break it down.

First we retrieve our relevant data sets:
--}

articlesByDate :: Connection -> Day -> Day -> IO [ArticleSummary]
articlesByDate conn from to = undefined

-- of course, we need us some SQL for that:

articlesByDateStmt :: Query
articlesByDateStmt = [sql|SELECT id,title,publish_dt FROM article
                          WHERE publish_dt >= ? AND publish_dt <= ?|]

-- Now, from those articles, get the associated Pivots...

articleSubjects :: Connection -> [Integer] -> IO [Pivot]
articleSubjects conn artIds = undefined

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
--}

type TopicalContext = FilePath -> [Subject] -> [Pivot] -> [ArticleSummary] -> IO ()

generateTopicalityCirclesReport :: TopicalContext
generateTopicalityCirclesReport jsonOutFile subjects pivots articles = undefined

generateTopicalityCSV :: TopicalContext
generateTopicalityCSV csvOutFile subjects pivots articles = undefined
