module Y2021.M03.D15.Solution where

import Control.Arrow (first)

import Data.Char (toLower, isAlphaNum)

import Data.List (replicate)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Tuple (swap)

{--
For #WomensHistoryMonth, let's look at the word-cloud of Grace Hopper's 
wikipedia entry (see image, included).

The wikipedia entry is here:
--}

graceHopperDir :: FilePath
graceHopperDir = "Y2021/M03/D15"

graceHopperArticle :: FilePath
graceHopperArticle = graceHopperDir ++ "/grace-hopper-wiki.txt"

-- The word-cloud was generated from d3.js. The URL is here:

wordCloudUrl :: FilePath
wordCloudUrl = "https://observablehq.com/@d3/word-cloud"

{--
As you can see from the javascript code, the stop-words were removed (words 
like 'the' and 'wasn't' (yes, 'wasn't' is a stop word. Go figure.)) But, can
we do better?

Sure. For example: 'computer' and 'computers', 'programming' and 'programs,'
and 'language' and 'languages' are all word-pairs, as you can see, but are
considered to be separate words. But that's stemming, and we're not going
to look at that today.

There are a lot of annotated-references in the wikipedia entry, so 'words' 
like '23' and '30' show up and, well: cloud the cloud.

How do we fix this?

Easy: you fix it, because that's today's Haskell problem. That's how you fix it.

You're welcome.

Okay.

First of all, let's pull in the stop words and filter the original document
down to a bag of words, minus the stop-words.
--}

stopWordsFile :: FilePath
stopWordsFile = graceHopperDir ++ "/stop-words.txt"

stopWords :: FilePath -> IO (Set String)
stopWords file = Set.fromList . lines <$> readFile file

{--
>>> sw <- stopWords stopWordsFile 
>>> sw
fromList ["a","about","above","after",...,"yourself","yourselves"]
--}

type Bag a = Map a Int

insert :: Ord a => a -> Bag a -> Bag a
insert a bag =
   Map.insert a (maybe 1 succ (Map.lookup a bag)) bag

wordBagFrom :: FilePath -> IO (Bag String)
wordBagFrom file = wordBag <$> readFile file

wordBag :: String -> Bag String
wordBag = foldr insert Map.empty . words . map toLower

{--
>>> wb <- wordBagFrom graceHopperArticle 
>>> length wb
1060
--}

-- what is the wordBag - stopWords? (n.b.: stop-words are all lowercase!)

moinsStopWords :: Set String -> Bag String -> Bag String
moinsStopWords stopwords = Map.filterWithKey (const . not . flip Set.member stopwords)

{--
>>> let smol = moinsStopWords sw wb
>>> length smol
962
>>n> take 10 $ Map.toList smol
[("\"",1),("\"debugging\"",1),("\"debugging.\"",1),("\"grandma",1),("\"had",1),
 ("\"hopper\"",1),("\"is",1),("\"it's",1),("\"nanoseconds\"",1),
 ("\"nanoseconds.\"[30]",1)]
--}

-- but, as you see in the above result, there are also a set of 'stop-words' 
-- of the form [x(x)(x)] which is to say: a reference is a word like: "[12]" 
-- that we don't care about. Get rid of the references.

moinsReferences :: Bag String -> Bag String
moinsReferences = mergeBag . map (first mr1) . Map.toList

mr1 :: String -> String
mr1 = reverse . mr2 . reverse

mr2 :: String -> String
mr2 l@(h:t) = if h == ']' then tail (dropWhile (/= '[') t) else l

-- n.b.: moinsReferences may create collisions. Ensure collisions are handled!

mergeBag :: [(String, Int)] -> Bag String
mergeBag = wordBag . uwb

unwordBag :: Bag String -> String
unwordBag = uwb . Map.toList

uwb :: [(String, Int)] -> String
uwb = unwords . concat . map (uncurry replicate . swap)

{--
>>> let nref = moinsReferences smol
>>> length nref
956

>>> take 10 $ Map.toList nref
[("",1),("\"",1),("\"debugging\"",1),("\"debugging.\"",1),("\"grandma",1),
 ("\"had",1),("\"hopper\"",1),("\"is",1),("\"it's",1),("\"nanoseconds\"",1)]
--}

-- now, let's eliminate punctuation, too, because ick.

-- but what are the punctuations? 

punctuation :: Bag String -> Set Char
punctuation =
     Set.delete '\''
   . Set.filter (not . isAlphaNum) 
   . Set.fromList 
   . concat
   . Map.keys

-- ... but not the apostrophe (') because that's stop-wordy.

{--
>>> let punct = punctuation nref
>>> punct
fromList "\"(),-.:;?[]\8211\8212"
--}

-- n.b.: ditto collision-protection for moinsPunctuation

moinsPunctuation :: Set Char -> Bag String -> Bag String
moinsPunctuation puncts =
     mergeBag
   . map (first $ filter (flip Set.notMember puncts))
   . Map.toList

{--
>>> let npun = moinsPunctuation punct nref
>>> length npun
848

Note that we've eliminated more than 100 redundancies already!

Now, that's all well and good, and gets us to a similar place to where word-
cloud is. But how about same-word words ("programming" and "programs," e.g.)?

That is where NLP, Neuro-linguistic Programming, or Natural Language Processing,
or Nutella Lattes with Peppermint Schnapps, comes in.

Because Nutella is yum, obvie.

*ahem*

Having sanitized our wiki article with the above moinsFoo functions (ya see
what I did there?) (or, perhaps you had to do yesterday's Haskell exercise to
get the implication), write out these cleaned words as a file:
--}

cleanedDoc :: FilePath -> Bag String -> IO ()
cleanedDoc file = writeFile file . unwordBag

{--
>>> cleanedDoc (graceHopperDir ++ "/cleaned.txt") npun
--}
