{-# LANGUAGE OverloadedStrings #-}

module Y2021.M02.D16.Solution where

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
import Y2021.M02.D08.Solution (extractReviews, fetchWineContext)
import qualified Y2021.M02.D15.Solution as Fixed

import Graph.Query (graphEndpoint)

import Control.Presentation   -- for Univ
import Control.Scan.CSV

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

import Data.Aeson
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)

-- ... we can down-cast to String, as we're hand-encoding these reviews.

dumpCSV :: FilePath -> [Review] -> IO ()

-- let's start simply and see what turns up.

dumpCSV outfile reviews = undefined

instance Univ Review where
   explode (Review rix wix rev mbs mbp) = 
      [show rix, show wix, dqshow rev, mbshow mbs, mbshow mbp]

mbshow :: Show a => Maybe a -> String
mbshow Nothing = ""
mbshow (Just a) = show a

dqshow :: Show a => a -> String
dqshow = ('"':) . dq' . tail . show

dq' :: String -> String
dq' [x] = [x] -- the trailing quote; don't process
dq' (h:h':t) = (if h == '\\' then unicodeMode h' else [h]) ++ dq' (h':t)

unicodeMode :: Char -> String
unicodeMode c | c == '"' = "\""
              | isDigit c = "\\u"
              | otherwise = "\\"

{--
>>> dqshow ("She said: \"They have a one-hour wait.\"")
"She said: \"\"They have a one-hour wait.\"\""
>>> putStrLn it
She said: ""They have a one-hour wait.""
--}

{--
Remember the hint last week about putStrLn. Also, embedded quotation-marks
in this CSV-format are represented not as (\") but as ("") ... so: yay!

Pensee:

The neat thing about this dumpCSV-function is that the ASCII-reviews are a
strict-subset of the unicoded-reviews.

We can dump all the reviews to this CSV, the load the all into the graph-
store using its own batch-ETL process.

Do that.

Unless, of course, you don't want to do that, in which case: create a CSV-file
of only the unicoded wine-reviews to be later uploaded to the graph-store.
--}

saveUnicodeReviews :: FilePath -> [Review] -> IO ()
saveUnicodeReviews outfile =
   writeFile outfile . unlines . (header:) . map uncsv

header :: String
header = "reviewer,wine,review,score,price"

{--
>>> graphEndpoint >>= fetchWineContext >>=
    extractReviews (Fixed.thisDir ++ "/parts/wine-reviews-fixed-aa-aa-aa-aa-aa-aa")
...
>>> let (ascis, unis) = it
>>> length unis
445
>>> saveUnicodeReviews "Y2021/M02/D16/unicoded-wine-reviews.csv" unis
--}

{--
Save the reviews into your graph store with something like:

load csv with headers from 'file:///unicoded-wine-reviews.csv' AS line
MATCH (t:Taster), (w:Wine)
WHERE id(t) = toInteger(line.reviewer) and id(w) = toInteger(line.wine)
CREATE (t)-[:RATES_WINE { review: line.review }]->(w)

You'll get a reply of something like:

Set 18615 properties, created 18615 relationships, completed after 2357 ms.

Checking one of the reviews shows:

match (t:Taster)-[r:RATES_WINE]->(w:Wine)
where id(t) = 55205 and id(w) = 147813
return r.review

Shows:

"). Phillip Zull's offering in the light, fresh Gr\u252ner Veltliner stakes is 
as light and fresh as they come. It's a burst of fruitiness across the palate, 
starring bright citrus and green fruits, with bone-dry acidity. Screwcap."

Shoot! \u252 is not unicode. \u0252 is. I don't quite know how to fix this :/

Maybe tomorrow we'll tackle that.
--}
