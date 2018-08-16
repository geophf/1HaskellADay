module Y2018.M08.D15.Solution where

{--
Today we have a list of the top 5000 English language words (by frequency) from
https://www.wordfrequency.info. We are going to parse this file and answer
a question.
--}

import Prelude hiding (Word)

import Control.Arrow ((&&&), second)

import Data.Array
import Data.Function (on)
import Data.List (intercalate, groupBy, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord

-- below modules available via 1HaskellADay git repository

import Control.Scan.CSV (rendBy)

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
readWordFreqs = 
   fmap (array (1,5000) . map (ixval2tup . fst . head) . filter (not . null)
       . map reads . lines) .
   readFile

data IxValue a = IxV Int a
   deriving Show

ixval2tup (IxV n v) = (n,v)

render :: String -> [String]
render = filter (not . null) . rendBy (\c -> c == ' ' || c == '\t')

instance Read a => Read (IxValue a) where
   readsPrec _ = ixit . render

ixit :: Read a => [String] -> [(IxValue a, String)]
ixit (h:t) = case reads h of
   [] -> []
   [(idx, _)] -> reads (intercalate "\t" t) >>= \(v, rest) ->
                 [(IxV idx v, rest)]
ixit [] = []

instance Read WordFreq where
   readsPrec _ = convert2WF . render

convert2WF :: [String] -> [(WordFreq, String)]
convert2WF [w,p,f,d] = reads f >>= \(freq,_) ->
                       reads d >>= \(disp,_) ->
                       return (WF w (head p) freq disp,"")
convert2WF _         = []

{--
>>> wrds <- readWordFreqs (exDir ++ tsvFile)
>>> length wrds
5000

Not bad for a little parser that could!

>>> take 5 (elems wrds)
[WF {word = "the", partOfSpeech = 'a', freq = 22038615, dispersion = 0.98},
 WF {word = "be", partOfSpeech = 'v', freq = 12545825, dispersion = 0.97},
 WF {word = "and", partOfSpeech = 'c', freq = 10741073, dispersion = 0.99},
 WF {word = "of", partOfSpeech = 'i', freq = 10343885, dispersion = 0.97},
 WF {word = "a", partOfSpeech = 'a', freq = 10144200, dispersion = 0.98}]
--}

-- What is the part-of-speech that has the most words? second most? etc?

partsOfSpeech :: Array Int WordFreq -> Map PartOfSpeech [Word]
partsOfSpeech = arr2map (partOfSpeech &&& word)

-- you can use a multi-map to construct the above result if you'd like

{--
>>> parts = map (second length) . Map.toList $ partsOfSpeech wrds
>>> sortOn (Down . snd) parts
[('n',2543),('v',1001),('j',838),('r',340),('i',97),('p',46),('c',38),('m',35),
 ('d',34),('u',13),('a',11),('x',2),('e',1),('t',1)]
--}

-- What are the words of length 5 in this list? Or, more generally, what are
-- the words of length n?

type Length = Int

nwords :: Array Int WordFreq -> Map Length [Word]
nwords = arr2map ((length &&& id) . word)

arr2map :: Ord a => (WordFreq -> (a, Word)) -> Array Int WordFreq -> Map a [Word]
arr2map fn = Map.fromList
           . map (fst . head &&& map snd) . groupBy ((==) `on` fst)
           . sortOn fst . map fn . elems

-- Again, use a multi-map if you'd like

{--
>>> length (nwords wrds Map.! 5)
841
>>> take 10 (nwords wrds Map.! 5)
["their","would","about","there","think","which","could","other","these","first"]
--}

-- Question: does the word-frequency obey Zipf's law?
