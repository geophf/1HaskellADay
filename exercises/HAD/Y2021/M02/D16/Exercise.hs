{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D16.Exercise where

{--
Last week, I had some frustration around unicoding. Let's pick up this torch
again, because, ya know, there's nothing like fixing unicoding ... for a system
that should be doing this for us, but yay.

Today's Haskell problem: we have the unicode-wine-reviews, instead of loading
them into the graph-store, incorrectly, let's write them out to a CSV-file
THEN upload the (hand-verified) unicoded file to the graph-store, directly.
--}

import Y2021.M02.D03.Solution (Review, Review(Review))
import qualified Y2021.M02.D03.Solution as WR

import qualified Data.Text as T

-- ... we can down-cast to String, as we're hand-encoding these reviews.

dumpCSV :: FilePath -> [Review] -> IO ()
dumpCSV = undefined

{--
Remember the hint last week about putStrLn. Also, embedded quotation-marks
in this CSV-format are represented not as (\") but as ("") ... so: yay!

Pensee:

The neat thing about this dumpCSV-function is that the ASCII-reviews are a
strict-subset of the unicoded-reviews.

We can dump all the reviews to this CSV, the load the all into the graph-
store using its own batch-ETL process.


Unless, of course, you don't want to do that, in which case: create a CSV-file
of only the unicoded wine-reviews to be later uploaded to the graph-store.
--}

saveUnicodeReviews :: FilePath -> [Review] -> IO ()
saveUnicodeReviews outfile unicodedReviews = undefined

{--
Save the reviews into your graph store with something like:

load csv with headers from 'file:///unicoded-wine-reviews.csv' AS line
MATCH (t:Taster), (w:Wine)
WHERE id(t) = toInteger(line.reviewer) and id(w) = toInteger(line.wine)
CREATE (t)-[:RATES_WINE { review: line.review }]->(w)

You'll get a reply of something like:

Set 18615 properties, created 18615 relationships, completed after 2357 ms.
--}
