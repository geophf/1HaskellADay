module Y2021.M03.D15.Exercise where

import Data.Map (Map)

import Data.Set (Set)
import qualified Data.Set as Set

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
considered to be separate words. Also, there are a lot of annotated-references
in the wikipedia entry, so 'words' like '23' and '30' show up and, well: cloud
the cloud.

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
stopWords = undefined

{--
>>> sw <- stopWords stopWordsFile
>>> sw
fromList ["a","about","above","after",...,"yourself","yourselves"]
--}

type Bag a = Map a Int

wordBagFrom :: FilePath -> IO (Bag String)
wordBagFrom file = undefined

wordBag :: String -> Bag String
wordBag = undefined

{--
>>> wb <- wordBagFrom graceHopperArticle 
>>> length wb
1060
--}

-- what is the wordBag - stopWords? (n.b.: stop-words are all lowercase!)

moinsStopWords :: Set String -> Bag String -> Bag String
moinsStopWords stopwords bagowords = undefined

{--
>>> let smol = moinsStopWords sw wb
>>> length smol
962
>>n> take 10 $ Map.toList smol
[("\"",1),("\"debugging\"",1),("\"debugging.\"",1),("\"grandma",1),("\"had",1),
 ("\"hopper\"",1),("\"is",1),("\"it's",1),("\"nanoseconds\"",1),
 ("\"nanoseconds.\"[30]",1)]
--}

-- but there are also a set of 'stop-words' of the form [x(x)(x)] which is
-- to say: a reference is a word like: "[12]" that we don't care about.
-- Get rid of the references.

moinsReferences :: Bag String -> Bag String
moinsReferences = undefined

-- n.b.: moinsReferences may create collisions. Ensure collisions are handled!

mergeBag :: Ord a => [(a, Int)] -> Bag a
mergeBag = undefined

{--
>>> let nref = moinsReferences smol
>>> take 10 $ Map.toList nref
[("",1),("\"",1),("\"debugging\"",1),("\"debugging.\"",1),("\"grandma",1),
 ("\"had",1),("\"hopper\"",1),("\"is",1),("\"it's",1),("\"nanoseconds\"",1)]
--}

-- now, let's eliminate punctuation, too, because ick.

-- but what are the punctuations? 

punctuation :: Bag String -> Set Char
punctuation bagowords = undefined

{--
>>> let punct = punctuation nref
>>> punct
fromList "\"(),-.:;?[]\8211\8212"
--}

-- ... but not the apostrophe (') because that's stop-wordy.

moinsPunctuation :: Set Char -> Bag String -> Bag String
moinsPunctuation puncts bagowords = undefined

{--
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
cleanedDoc = undefined

-- tomorrow we'll look at using Natural Language Processing for a cleaner
-- word-cloud.
