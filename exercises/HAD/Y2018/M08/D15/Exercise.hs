module Y2018.M08.D15.Exercise where

{--
Today we have a list of the top 5000 English language words (by frequency) from
https://www.wordfrequency.info. We are going to parse this file and answer
a question.
--}

import Prelude hiding (Word)
import Data.Array
import Data.Map

-- below modules available via 1HaskellADay git repository

import Control.Scan.CSV
import qualified Data.MultiMap as MM

exDir, tsvFile :: FilePath
exDir = "Y2018/M08/D15/"
tsvFile = "words_counts.tsv"

-- The TSV file has a header and footer, so watch out. Parse the file into
-- the following structure:

type Word = String
type PartOfSpeech = Char

data WordFreq = WF { word :: Word, partOfSpeech :: PartOfSpeech,
                     freq :: Int, dispersion :: Float }
    deriving Show

-- the parts of speech are catalogued here: ... um ... okay, bad link.
-- deal with it: guess away.

-- So, our word frequencies:

readWordFreqs :: FilePath -> IO (Array Int WordFreq)
readWordFreqs file = undefined

-- What is the part-of-speech that has the most words? second most? etc?

partsOfSpeech :: Array Int WordFreq -> Map PartOfSpeech [Word]
partsOfSpeech wordfreqs = undefined

-- you can use a multi-map to construct the above result if you'd like

-- What are the words of length 5 in this list? Or, more generally, what are
-- the words of length n?

type Length = Int

nwords :: Array Int WordFreq -> Map Length [Word]
nwords wordfreqs = undefined

-- Again, use a multi-map if you'd like
