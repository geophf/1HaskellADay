module Y2018.M01.D26.Exercise where

{--
Today's Haskell problem is broken up into two-parts: we're going to be building
an app that downloads a set of articles from a REST endpoint and then upload
the articles to a PostgreSQL data store, but only those articles that are new
or updated.

How do we do the latter? We'll look at that tomorrow.

How do we do the former? Well, we'll eyeball it.

WHY will we eyeball it? eh.

* 'eh' is a precise scientific term of measure.

Or, less 'eh'-ily: we don't know what articles are new or updated, but MOST
new articles or updated articles occur within the last week, so we'll download
a week's worth of articles and then worry about which ones are new or updated
tomorrow.

TODAY's exercise is to download a week's-worth of articles from a REST endpoint.
--}

import Data.Time
import Data.Time.Clock

-- of course, we've accessed the REST endpoint before so:

import Y2018.M01.D16.Exercise (readPacket)

-- So now we need to download packets until we've gone back to a week of them:

import Y2017.M12.D20.Exercise (Packet, rows)
import Y2017.M12.D27.Exercise (DatedArticle, starttime, lastupdated)

-- how many packets constitute a week's-worth of articles?

oneWeek :: IO [Packet]
oneWeek = undefined
