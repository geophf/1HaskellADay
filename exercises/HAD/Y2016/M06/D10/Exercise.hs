module Y2016.M06.D10.Exercise where

{--
@1HaskellADay exercise for 2016-06-10

Quartiles.

We're going to be looking at the 'box-and-whiskers' also known as the 
'candlesticks' charting method of grouping (and separating) data.

Box and whiskers is ... okay, you can read wikipedia

https://en.wikipedia.org/wiki/Box_plot

So (now that you've read the whole article and are SO ENLIGHTENED!) the
box and whiskers divides the data set into two along the median, then divides
the high and low sets along their media so that you have four data sets. These
sets are called quartiles.
--}

import Data.Graphics.SVG              -- http://lpaste.net/309926248828633088

-- Given: a set of data

setOdata :: [Int]
setOdata = [28,40,10,15,38,24,13,52,33,49,21,35,46,16,37]

-- Divide the set into 4 quartiles.

quartiles :: Num a => [a] -> [[a]]
quartiles = undefined

-- What can you say about the dispersion of the data in quartile?
-- What is a good measure to help show dispersion of a quartile, in the context
-- of all the quartiles or of the entire data set?

{-- BONUS -----------------------------------------------------------------

Output the set of data as a box-and-whiskers chart. You can use your spreadsheet
or d3js.org or SVG for your representation. Or ... whatever you are comfortable
with: stock analyses packages have candlestick charts, too, so ... go to town!
--}

boxAndWhiskersChart :: Num a => [[a]] -> IO ()
boxAndWhiskersChart = undefined
