module RID.Parser where

{-- a solution to the problem posted at http://lpaste.net/237534433320632320

Okay, YESTERDAY, we ingested the entire cognitive sets of the RID: PRIMARY,
SECONDARY, and EMOTIONS. Great. Wonderful.

YAY!

Give yourselves high-fives.

And I have to formalize ('formalise'?) this in the module and source data sets
still. (On it.)

But, so what? What can you do with this amassed information? How is it useful?

We won't answer these questions today, but we will take just one brick on the
wall and look at it.

RootWord.

So, you have this thing, this RootWord, and, in your source documents you have
... well, a stream of characters, some are letters, some are spaces, some are
punctuation marks, some are, ... eh: emojis, and none of them, DEFINITELY, are
structured exactly like our RootWords are ... unless, of course, you're parsing
the RID, itself, and so, good on you, for exactly one document, on that.

So, today, let's ingest a source document, break it up into words, remove all
the non-alphas from those words, and then match them to the RootWords ...
Not all of them, and not reposed in the RID. Oh, no. Let's digest this
elephant one bite at a time.

Okay, SO! ... (I say the word 'so' a lot, ICYMI) ... you have the following
document:
--}

import Control.Monad ((>=>), liftM)
import Data.Char (isAlpha, toUpper)
import Data.Graph
import Data.List (isPrefixOf, sortBy)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Bag (Bag)
import qualified Data.Bag as Bag
import RID.Graph
import RID.Tree

qwerty :: String
qwerty = "The quick, brown fox jumps over the lazy dog."

-- and you have the following (root-)word-set:

wordset :: Category
wordset =
   fst $ parseWordSet [[(1, "SAMPLE")], [(2, "THE (1)"), (2, "QUICK* (1)"),
                        (2, "BROWN* (1)"), (2, "FOX* (1)"), (2, "JUMP* (1)"),
                        (2, "OVER* (1)"), (2, "LAZ* (1)"), (2, "DOG* (1)")]]

-- Write a function that, given a category and an input document (as a String)
-- returns a set of Path values

wordsFromWS :: Category -> Set RootWord
wordsFromWS = Set.fromList . Map.elems . roots
    -- dependent type to avoid rt error

type Document = String

matcher :: RID -> Document -> Bag Path
matcher rid =
   let graphInfo = rid2graph rid
       wrds      = graphRoots graphInfo
   in  Bag.fromList . mapMaybe (flip Map.lookup wrds . liftRW) . convertText

{-- So we need a map from a word to a root-word ... a matching-mapping funcion.

*RID.Parser Data.Time.Clock> getCurrentTime >>= \start ->
            print (last (Bag.rank (matcher rid pnp))) >>
            getCurrentTime >>= return . flip diffUTCTime start
(["PRIMARY","REGR_KNOL","CONCRETENESS","AT (1)"],Sum {getSum = 800})
4.54235s

--}

convertText :: String -> [String]
convertText = map (filter isAlpha . map toUpper) . words

-- *Main> convertText qwerty ~>
-- ["THE","QUICK","BROWN","FOX","JUMPS","OVER","THE","LAZY","DOG"]

-- hints: all root words are capitalized. All root words have no punctuation.

-- matching rules by example
-- The RootWord FOX* matches FOX (of course) and FOXES and FOXYLADY, too
-- The RootWord THE (no '*' or wildcard) matches only THE, not THEY nor THESIS

{-- 
Question: What is the most pervasive path in qwerty using wordset?

*Main> matcher wordset qwerty ~>
{(["SAMPLE","BROWN* (1)"],Sum {getSum = 1}),
 (["SAMPLE","DOG* (1)"],Sum {getSum = 1}),
 (["SAMPLE","FOX* (1)"],Sum {getSum = 1}),
 (["SAMPLE","JUMP* (1)"],Sum {getSum = 1}),
 (["SAMPLE","LAZ* (1)"],Sum {getSum = 1}),
 (["SAMPLE","OVER* (1)"],Sum {getSum = 1}),
 (["SAMPLE","QUICK* (1)"],Sum {getSum = 1}),
 (["SAMPLE","THE (1)"],Sum {getSum = 2})}

*Main> head . sortBy (comparing (Down . snd)) $ Map.toList it ~>
(["SAMPLE","THE (1)"],Sum {getSum = 2})

So ... apparently, the fox says 'THE'! ... twice!

Now let's try this again with the full RID:

*RID.Parser> readinRID "RID" ~> rid
*RID.Parser> matcher rid qwerty ~>
{(["PRIMARY","DEFENSIVE_SYMBOL","PASSIVITY","LAZY* (1)"],Sum {getSum = 1}),
 (["PRIMARY","ICARIAN_IM","ASCEND","JUMP* (1)"],Sum {getSum = 1}),
 (["PRIMARY","REGR_KNOL","CONCRETENESS","OVER* (1)"],Sum {getSum = 1}),
 (["PRIMARY","SENSATION","VISION","BROWN* (1)"],Sum {getSum = 1}),
 (["SECONDARY","TEMPORAL_REPERE","QUICK* (1)"],Sum {getSum = 1})]

Cool beans!
--}
