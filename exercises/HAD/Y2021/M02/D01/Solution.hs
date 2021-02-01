module Y2021.M02.D01.Solution where

import Network.HTTP.Conduit
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (isSuffixOf)

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

collectDupeRows :: FilePath -> IO [ByteString]
collectDupeRows url =
   filter (not . isSuffixOf "," . BL.unpack) . BL.lines <$> simpleHttp url

-- How many rows is that? What do these rows look like?

{--
>>> collectDupeRows wineReviewsCSV 
["id,country,description,designation,points,price,province,region_1,...",...]
>>> let dupeless = it
>>> length dupeless
5

... the first row, the header, doesn't count.
>>> mapM_ BL.putStrLn (tail dupeless)
51394,US,"Brisk and clean, this dry white is the 
18881,South Africa,"Notes of cocoa and oak-derived spice add depth to the ripe, dark fruit core of this wine. Black berry, cherry and plum flavors abound on the plush palate, framed by firm tannins and a thick, supple texture. Gripping plum-skin and cigar-box notes linger long on the finish.","Grand Classique Cabernet Sauvignon-Petit Verdot-Malbec-
Merlot-Cabernet Franc",90,20,Paarl,,,Lauren Buzzeo,@laurbuzz,"Glen Carlou 2009 Grand Classique Cabernet Sauvignon-Petit Verdot-Malbec-
21516,Italy,"This offers generous tones of cherry, dried raspberry, moist tobacco, cured meat and white truffle. It is 

... this is more indicative of formatting issue more than data-duplication. :/
--}
