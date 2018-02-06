module Y2018.M02.D08.Exercise where

{--
Okay, we've updated articles from the triage of articles we've downloaded from
our REST endpoint. Today we shall insert the new articles into the database
from that same triaged set.

We've done this before: inserting articles, so this should be a snap, yes?

So, we'll do that (tying together packet information, too) and integrate the
updating process we defined yesterday into a big-ol' mondo insertAndUpdate
function.
--}

import Database.PostgreSQL.Simple

-- below imports available via 1HaskellADay git repository

import Data.Logger
import Data.Stamped

import Y2017.M12.D27.Exercise (DatedArticle)

import Y2018.M01.D04.Exercise (Authors)
import Y2018.M01.D30.Exercise (Triage, ArticleTriageInformation)

-- so, we need to figure out how to get from the ArticleTriageInformation
-- to a set of indexed articles (that return block indices) then to a set
-- of indexed blocks ... make sure the articles are sorted going in so that
-- the sorted results coming out match the block ids (I learned that one the
-- hard way!

insertAndUpdate :: Connection -> Map Triage [ArticleTriageInformation]
                -> StampedWriter LogEntry [IxValue (DatedArticle Authors)]
insertAndUpdate conn triage = undefined
