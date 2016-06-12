{-# LANGUAGE ViewPatterns #-}

module Y2016.M06.D10.Solution where

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

import Control.Arrow ((&&&), (***), second)
import Data.List (sort)

import Data.Graphics.SVG              -- http://lpaste.net/309926248828633088

-- Given: a set of data

setOdata :: [Int]
setOdata = [28,40,10,15,38,24,13,52,33,49,21,35,46,16,37]

-- Divide the set into 4 quartiles.

data Quartile t a = Q { low :: t a, median :: a, hi :: t a } deriving Show

-- here's a clever trick: if we make Quartile a Foldable then we can have
-- quartiles of quartiles ... oh, yeah!

-- so first, let's just divide the data set
-- n.b.: divide presupposes data is already sorted.

divide :: (Ord a, Num a) => [a] -> Quartile [] a
divide = uncurry uncurry
       . (Q *** (head &&& tail))
       . uncurry splitAt
       . ((`div` 2) . length &&& id)

{--
*Y2016.M06.D10.Solution> divide (sort setOdata) ~>
Q {low = [10,13,15,16,21,24,28], median = 33, hi = [35,37,38,40,46,49,52]}

Question: What does it say about me, as a person, that I actually wrote:

'uncurry uncurry'

with a very sincere, ardent expression on my face.

Asking for a friend.
--}

-- Of course 'quartile of quartiles' is comonadic... just sayin'

quartiles :: (Ord a, Num a) => [a] -> Quartile (Quartile []) a
quartiles (divide -> q) = Q (divide (low q)) (median q) (divide (hi q))

{--
*Y2016.M06.D10.Solution> quartiles (sort setOdata )
Q {low    = Q {low = [10,13,15], median = 16, hi = [21,24,28]}, 
   median = 33,
   hi     = Q {low = [35,37,38], median = 40, hi = [46,49,52]}}
--}

-- What can you say about the dispersion of the data in quartile?
-- What is a good measure to help show dispersion of a quartile, in the context
-- of all the quartiles or of the entire data set?

{-- BONUS -----------------------------------------------------------------

Output the set of data as a box-and-whiskers chart. You can use your spreadsheet
or d3js.org or SVG for your representation. Or ... whatever you are comfortable
with: stock analyses packages have candlestick charts, too, so ... go to town!

Okay, from d3js.org there's a link off to:

http://bl.ocks.org/mbostock/4061502

That draws box-charts from the following format:

Expt,Run,Speed
1,1,850
1,2,740
1,3,900
1,4,1070
1,5,930

So, let's output our unsorted data set to a CSV-file and run it through the
Javascript and see what we see:
--}

boxAndWhiskersChart :: Show a => FilePath -> [a] -> IO ()
boxAndWhiskersChart file = writeFile file . unlines
   . ("Expt,Run,Speed":) . map showOne . zip [1..]

showOne :: Show a => (Int, a) -> String
showOne (a,b) = "1," ++ show a ++ (',':show b)

{--
*Y2016.M06.D10.Solution> mapM_ (putStrLn . showOne) (zip [1..] setOdata)
1,1,28
1,2,40
1,3,10
1,4,15
1,5,38
1,6,24
1,7,13
1,8,52
1,9,33
1,10,49
1,11,21
1,12,35
1,13,46
1,14,16
1,15,37

But, actually, however, the source material is not CSV/AJAX-friendly, so for now
I (embarrassedly) copied the array into the sample calling-HTML page and got
the pretty box-chart from the unsorted data as a javascript array.

It's a shame to do all the work of computing the quartiles here and then 
discarding that information so that the javascript can recompute it all again
to display the box-chart. The javascript code is poorly factored and relies
on global state, so teasing out the drawing part is hard for me in my 
unfamiliarity with javascript. Perhaps someone wants to take a stab at this?
--}
