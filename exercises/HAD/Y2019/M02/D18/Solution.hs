module Y2019.M02.D18.Solution where

{--

Given a set of letters and a word-dictionary (that you locate), come up with
a set of 3-letter words, 4-letter words, ... n-letter words from those letters.

Rule: you can use each letter only once.

So, for example, given:

"meiosp"

The following (and more) words are returned:

[["sop","sip","sim","pie"],["some","mops","pose"],["poise"]]

Do not return 1- or 2-letter words.

I have a word-dictionary on my computer I use, you may find your own
--}

import Control.Arrow ((&&&))

import Data.Char (toLower) -- ... this might help
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid (Sum)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

-- below imports available via 1HaskellADay git repository

import Data.Bag (Bag)
import qualified Data.Bag as Bag

wordDictionary :: FilePath
wordDictionary = "/usr/share/dict/words"

-- Problem 1: return 3+-letter words

type WordsByLength = Map Int (Set String)
type Dict = WordsByLength

wordsByLength :: FilePath -> IO Dict
wordsByLength = fmap (sortem . filter ((> 2) . length) . words) . readFile

sortem :: [String] -> Dict
sortem = Map.fromList
       . map (length . head &&& Set.fromList)
       . groupBy ((==) `on` length)
       . sortOn length
       . map (map toLower)

-- returns { (3, set of three-letter words), ...}

{--
>>> dict <- wordsByLength wordDictionary 
>>> length dict
22
>>> Map.keys dict
[3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]
--}

wordsFrom :: Dict -> String -> WordsByLength
wordsFrom dict letters =
   fromMaybeList 
       (map (fmap (length . fst) . Set.minView &&& id)
            (mapMaybe (`Map.lookup` dict) [3 .. length letters] >>=
              return . Set.filter (wordContainsOnly letters)))

fromMaybeList :: Ord a => [(Maybe a,b)] -> Map a b
fromMaybeList = Map.fromList . mapMaybe filterOnFst

filterOnFst :: (Maybe a,b) -> Maybe (a,b)
filterOnFst = fmap swap . sequence . swap

{--
>>> ws = wordsFrom dict "meiosp"
>>> length ws
4
>>> ws
{(3,{"imp","ism","iso","meo","mes","moe","moi","mop","oes","ope","ose","pes",
     "pie","pim","poe","poi","pom","psi","sie","sim","sip","soe","sop"}),
 (4,{"epos","meio","meso","mise","mope","mose","oime","peso","pise","piso",
     "poem","pome","pose","semi","sime","simp","sipe","some","sope"}),
 (5,{"epsom","moise","poise"}),
 (6,{"impose"})}
--}

-- this function may help:

wordContainsOnly :: String -> String -> Bool
wordContainsOnly ltrs =
   all (smol (Bag.fromList ltrs)) . Map.toList . Bag.fromList

-- smol ensures there are less-ish characters than in the letter-list

smol :: Bag Char -> (Char, Sum Int) -> Bool
smol dict (chr,n) = fromMaybe False (fmap (n <=) (Map.lookup chr dict))

{-- 
Returns true if word uses some of the characters of letters only once, so:

>>> wordContainsOnly "meiosp" "hello"
False
>>> wordContainsOnly "meiosp" "me"
True
>>> wordContainsOnly "meiosp" "mess"
False
--}
