{-# LANGUAGE OverloadedStrings #-}

module Y2017.M09.D15.Exercise where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import Network.HTTP.Conduit

-- below imports available via 1HaskellADay git repository

import Y2017.M09.D08.Exercise
import Y2017.M09.D13.Exercise (articlesDir)

{--
Today we are going to build a KEA, a Keyword Extraction Algorithm, a simple one,
but one, none-the-less. Our approach will be thus:

take an article, 
rend it into individual words,
get the total word count for that article
get the word count for each unique word in the article
compute the strength of each word of the article
--}

data KeyWord = KW { kwId, count :: Int, strength :: Float }
   deriving (Eq, Ord, Show)

-- a keyword has an id, a count in the document, and its relative strength

type Dictionary = Map String Int

-- a dictionary is a list of keywords and their ids

kea :: Dictionary -> ByteString -> (Dictionary, Map Int KeyWord)
kea dict file = undefined

-- kea scans a file, assigning a count and weight to each keyword. If the
-- keyword isn't in the dictionary, create a new entry with that keyword

-- A helper function might be ... well, helpful:

data WordContext =
   WC { dict :: Dictionary, kws :: Map Int KeyWord, index :: Int }
      deriving Show

kea' :: WordContext -> Int -> String -> WordContext
kea' context wordCount word = undefined

-- given a dictionary and a set of keywords, update the dictionary with a new
-- word (and add that word's key to the keyword index), or update the keyword
-- index if the word is already in the dictionary.

-- when you've defined the above, what is the keyword set of:

testFile :: FilePath
testFile = articlesDir ++ "AP900327-0094.txt"

{-- BONUS -----------------------------------------------------------------

That's all fine and dandy for just one article, but what happens when you
analyze a set of articles? Compute the set of keywords for all the articles
in articlesDir
--}

keywordsAllArticles :: URL -> Directory -> IO (Map Int KeyWord)
keywordsAllArticles url dir = undefined
