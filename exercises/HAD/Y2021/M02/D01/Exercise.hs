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
--}

collectDupeRows :: FilePath -> IO [String]
collectDupeRows = undefined

-- How many rows is that? What do these rows look like?
