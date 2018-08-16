module Y2018.M08.D16.Exercise where

-- Question: does the word-frequency obey Zipf's law?

import Prelude hiding (Word)

import Control.Arrow (first)

import Data.Array (Array)
import Data.Map (Map)
import Data.Set (Set)

import Y2018.M08.D15.Exercise

-- Given the words and their counts that we scanned yesterday, prove or
-- disprove Zipf's Law. Show your work

-- Zipf's Law: https://en.wikipedia.org/wiki/Zipf's_law

zipfLaw :: Array Int WordFreq -> Bool
zipfLaw wordsfreqs = undefined

{--
>>> wrds <- readWordFreqs (exDir ++ tsvFile)
>>> length wrds
5000

>>> zipfLaw wrds
True
--}

-- It is said by some wise old philosophers philosophizing that all you need is
-- 2000 words to express yourself in a language. How accurate is that statement?
-- Using everyday sentences found anywhere you'd like and the wordsfreqs, show
-- which words of those sentences are in the wordsfreqs corpus.

-- metaquestion: is 'corpus' in the corpus?
-- metametaquestion: is 'metaquestion' in the corpus?

type Sentence = Set Word

data Analysis = Analyze { inCorpus :: Map Word Int, notInCorpus :: Set Word }
   deriving Show

analyzeSentence :: Array Int WordFreq -> Sentence -> Analysis
analyzeSentence corpus sentence = undefined

-- returns the index in the corpus of each word of that sentence.

-- So, of course you need to map words to the indices, right?

reverseMap :: Array Int WordFreq -> Map Word Int
reverseMap corpus = undefined

samples :: [(Sentence, FilePath)]
samples = map (first mkSentence) [
   ("She wonders if she is allowed tea.",
    "https://twitter.com/Sir_Creaky/status/1029988009133920256"),
   ("Damn, women wear too much makeup 💄this decade. Y’all are beautiful without the highlighter and contour.",
    "https://twitter.com/6CentsRose/status/1029970902119641089"),
   ("Vàrpalota,Thury Castle.#Hungary. Have a nice day everyone. God Bless You All and Your Loved Ones!",
    "https://twitter.com/Threepo1/status/1030026773147398144"),
   ("Just lost a ton of followers, was it something I said",
    "https://twitter.com/xTrisarahtops/status/1029882782909124608")]

mkSentence :: String -> Sentence
mkSentence sentence = undefined

-- reminder: capitalization, punctuation and special characters: deal.
