module Y2017.M01.D26.Solution where

import Data.Function (on)
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (getSum)
import Data.Ord

-- below imports available from 1HaskellADay git repository

import Data.Bag (Bag, add, emptyBag)
import Y2017.M01.D25.Solution

{--
So, yesterday we were able to find out what Haskell source files were in a
directory, then, as a bonus, we were also able to drill down into subdirectories.

Great!

Now, today, let's do a frequency analysis of the words of a Haskell file.
--}

wordCounts :: FilePath -> IO (Map String Int)
wordCounts = fmap (toInt . wc' emptyBag) . readFile

toInt :: Bag a -> Map a Int
toInt = Map.map getSum

wc' :: Bag String -> String -> Bag String
wc' bag = foldr add bag . words

-- wordCounts counts the words of a (Haskell) source file returning a
-- word -> occurences map.

-- hint: Data.Bag counts occurences of elements in a collection

-- Point wordCounts at this file. What are the top 5 words in this file?

{--
*Y2017.M01.D26.Solution> wordCounts "Y2017/M01/D26/Exercise.hs" ~> counts
*Y2017.M01.D26.Solution> take 5 (sortBy (compare `on` Down . snd) (Map.toList counts)) ~>
[("a",9),("the",9),("of",8),("--",5),("Haskell",5)]

The words 'a' 'the' tie for first, 'of' is right after that. Now it also 
counts 'non-words' and we may wish to exclude these.
--}

{-- BONUS -----------------------------------------------------------------

Now, one file doesn't give a good cross section of frequently used words
in the Haskell corpus, so, find a Haskell Corpus, such as the sources of
the GHC libraries, or the 1HaskellADay problem sets and libraries, or your
own sets of Haskell files.

Run wordCounts over those filesets. What are the top 5 words of the combined
files?
--}

wordsCounts :: [FilePath] -> IO (Map String Int)
wordsCounts = fmap (toInt . foldr (flip wc') emptyBag) . mapM readFile

{--
*Y2017.M01.D26.Solution> haskellFilesR "." ~> files ~> length ~> 409
*Y2017.M01.D26.Solution> wordsCounts files ~> bigcounts ~> length ~> 48281
*Y2017.M01.D26.Solution> take 5 (sortBy (compare `on` Down . snd) (Map.toList bigcounts))
[(",",26859),("|",14083),("County",12062),("the",4283),("=",4205)]

AHA! My word-counts results differ from 'them.' Just goes to show that I'm
not addicted to the 'use a to represent any type'-haskell-programming-style.

So there! ;)
--}
