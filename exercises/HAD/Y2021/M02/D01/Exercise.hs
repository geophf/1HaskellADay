module Y2021.M02.D01.Exercise where

import Network.HTTP.Conduit

{--
So, I have a problem. I want to upload the wine-reviews to my graph-store,
but, in the CSV file of data:
--}

wineReviewsCSV :: FilePath
wineReviewsCSV = "https://raw.githubusercontent.com/lju-lazarevic/wine/master/data/winemag-data-130k-v2.csv"

{--
... there's this final column called 'dupe?'

What is that?

Collect the rows in this data-set that are flagged as 'dupe?'

So, first of all:

>>> length . BL.lines <$> simpleHttp wineReviewsCSV
119993

So there some things to get through.
--}

collectDupeRows :: FilePath -> IO [String]
collectDupeRows = undefined

-- How many rows is that? What do these rows look like?
