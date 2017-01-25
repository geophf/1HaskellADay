module Y2017.M01.D26.Exercise where

import Data.Map (Map)

-- below imports available from 1HaskellADay git repository

import Data.Bag
import Y2017.M01.D25.Exercise

{--
So, yesterday we were able to find out what Haskell source files were in a
directory, then, as a bonus, we were also able to drill down into subdirectories.

Great!

Now, today, let's do a frequency analysis of the words of a Haskell file.
--}

wordCounts :: FilePath -> IO (Map String Int)
wordCounts file = undefined

-- wordCounts counts the words of a (Haskell) source file returning a
-- word -> occurences map.

-- hint: Data.Bag counts occurences of elements in a collection

-- Point wordCounts at this file. What are the top 5 words in this file?

{-- BONUS -----------------------------------------------------------------

Now, one file doesn't give a good cross section of frequently used words
in the Haskell corpus, so, find a Haskell Corpus, such as the sources of
the GHC libraries, or the 1HaskellADay problem sets and libraries, or your
own sets of Haskell files.

Run wordCounts over those filesets. What are the top 5 words of the combined
files?
--}

wordsCounts :: [FilePath] -> IO (Map String Int)
wordsCounts files = undefined
