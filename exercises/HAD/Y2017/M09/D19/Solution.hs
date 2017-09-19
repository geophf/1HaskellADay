{-# LANGUAGE ViewPatterns #-}

module Y2017.M09.D19.Solution where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Ord                    -- for (Down)
import Data.Tuple (swap)

-- below import available via 1HaskellADay git repository

import Y2017.M09.D15.Solution

{--
Today is the reverse from before ... or, not 'reverse,' but 'dual.'

Or something.

Say you have from Y2017.M09.D15.Exercise a set of keywords from documents,
encoded into integers. Well, today, we want to know something. We already know
for some keyword, k, what integer represents it:

Map.lookup keywordMap k

gives us that.

But what about the other way?

Today, we need to know for some set of integers, x, what are the keywords?

Why? BECAUSE I SAID SO!

I think there's some business case for when you're handed a bunch of keywords
encoded as integers to know what those integers represent, right?

Okay, so if we scan the elems and link back to the key, we've got ourselves
a linear search, each time. So, let's write a reversed dictionary values-to-
keys and use that to define lookupWord.
--}

transposeDict :: Dictionary -> Map Int String
transposeDict = Map.fromList . map swap . Map.toList

flipLookId :: Map Int String -> KeyWord -> Maybe String
flipLookId dict = (`Map.lookup` dict) . kwId

lookupWord :: Dictionary -> KeyWord -> Maybe String
lookupWord (transposeDict -> dict) = flipLookId dict

-- given a mapping (string -> int) and a keyword (declared in Y2017.M09.D15)
-- return the word associated with the keyword encoding.

{--
>>> wc <- kea 0 Map.empty <$> BL.readFile testFile
>>> lookupWord (dict wc) (snd (head (Map.toList (kws wc)))) 
Just "the"
>>> lookupWord (dict wc) (snd (head (tail (Map.toList (kws wc)))))
Just "census"
--}

-- Now from the article repository that you've encoded into keywords, what
-- are the top five words used in that article repository?

top5Words :: Dictionary -> Map Int KeyWord -> [String]
top5Words (transposeDict -> dict) =
   mapMaybe (flipLookId dict) . take 5 . sortOn (Down . strength) . Map.elems

{--
>>> top5Words (dict wc) (kws wc)
["the","lines","to","census","a"]
--}
