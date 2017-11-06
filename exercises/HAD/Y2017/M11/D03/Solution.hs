{-# LANGUAGE ViewPatterns #-}

module Y2017.M11.D03.Solution where

{--
So, just got a metric tonne of new information from the NYT article archive.
Last Thursday (Y2017.M10.D26) we used artificial-artificial intelligence to
analyze a set of articles for topicality. Today, we've got results from key
phrase extraction algorithms to identify the topicality for sets of articles.

The format, including all its beautious noise is here as kw_index_file.txt
--}

import qualified Codec.Compression.GZip as GZ
import Control.Arrow (first)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map

kwDir :: FilePath
kwDir = "Y2017/M11/D03/"

{--
You'll see, when you decompress the file, that the format is thus:

[id1, [(strength, 'key phrase')]], [id2, ...], ...

The information here is a bit different than what we have in our data store,
however.

1. The id's are 0-based, and do not have a one-to-one correspondence to the
   ids in the data store.
2. The full text is in a list and single quoted, also, special characters are
   removed

so, how do we match the key phrases to the articles in the database so we
can store them there?

One way is to get ids of this data set to match the ids in the data store.

YA THINK?

Another is to create a matched-pair of ids to ids.

Let's do the latter, because I don't have time to wait for the former.

Ah, the life of a start-up, where people cut corners to save time and end up
wasting so much time by hurrying up.

So, first up, we're going to create an in-memory pivot table that translates
from this data set's ids to ...

No. No. We are going to wait until genius matches the ids to the data store
THEN we're going to store the keywords.

... 4 days later.

Finally got a data set where the ids match the data-store ids. So let's proceed

... but only after the drama of me explaining for THREE DAYS why I need ids to
match, then, on the fourth day, the boss-man saying: "Why don't the ids match?
They need to match! How can I do my queries if the ids don't match?"

Yeah. Great. Glad you're on-board with matching ids now.

ANYWAY.

We have several interesting problems with this data set:

1. it's a set of raw 'lists' ... I say 'lists' because the list elements vary
in types (so it's more like a map or array)
2. The strings are single-quoted.

So we can't just do a 

map read . lines

and call it a day.

OR CAN WE?

Hm.
--}

data SingleQuotedString = SQS { string :: String }
   deriving (Eq, Ord, Show)

instance Read SingleQuotedString where
   readsPrec _ = bracketed '\'' '\'' SQS
   readList = bracketed '[' ']' liststrings

liststrings :: Read a => String -> [a]
liststrings [] = []
liststrings (break (== ',') -> (elt, rest)) =
   read elt : liststrings (trim (softtail rest))

softtail :: String -> String
softtail [] = []
softtail (_:t)= t

{--
>>> (read "'european union citizens living'") :: SingleQuotedString
SQS "european union citizens living"
--}

type Strength = Double
data Keyword = KW Strength SingleQuotedString
   deriving (Eq, Ord, Show)

sampleKeyword :: String 
sampleKeyword = "(9.0, 'subject line .)')"

{--
>>> (read sampleKeyword) :: Keyword
KW 9.0 (SQS {string = "subject line .)"})
--}

sampleKeywordList :: String
sampleKeywordList = "[(12.25, 'state department would say'), "
                  ++ "(12.236813186813185, 'pifer said american diplomats')]"

{--
>>> (read sampleKeywordList) :: [Keyword]
[KW 12.25 (SQS {string = "state department would say"}),
 KW 12.236813186813185 (SQS {string = "pifer said american diplomats"})]
--}

instance Read Keyword where
   readsPrec _ = map (first (uncurry KW)) . forceOrder

{--
fml on readList. seriously.
--}
   readList = bracketed '[' ']' listkws

forceOrder :: Read n => String -> [((n, SingleQuotedString), String)]

-- okay, so keyword tuples are coming at us either (strength, sqs) OR (sqs, strength)
-- WTF? Okay, whatever. So we just ask our parser to deal with it.

forceOrder str@(_openParen:first:rest) | first == '\'' = -- inverted structure: (sqs, strength)
   let (str1, _quot:_comma:numParen) = break (== '\'') rest
       (num,_closeParen:restRest) = break (== ')') (trim numParen) in
   [((read num, SQS str1), restRest)]
                            | otherwise     = -- (n, 'quot') order
   let (num, _comma:str1) = break (== ',') (tail str)
       (str2, _quot1:_closeParen:restRest) = break (== '\'') (tail (trim str1)) in
   [((read num, SQS str2), restRest)]

listkws :: String -> [Keyword]
listkws [] = []

{--
listkws (break (== ')') -> (el, r:est)) =

-- we're effed if the keyword contains parentheses

   -- error ("read in " ++ el ++ [r]) :
   read (el ++ [r]) : listkws (trim (softtail est))

No, that didn't work, because there may be ... and there actually are ...
embedded parenthesized expressions in the singly-quoted strings. So, we
have to parse the elements, one-by-one:
--}

listkws listElts = 

{--
Given forceOrder ...
-- first, we read the num:

   let (_openParen:n,_comma:rest) = break (== ',') listElts

-- now rest has the singly-quoted string and the rest of the list, so we
-- have to parse to the end of the singly-quoted string

       (str,_quot:_closeParen:rest') = break (== '\'') (softtail $ trim rest)
--}
   let [(ans, rest')] = forceOrder listElts

-- now, rest' may have a comma, or may be the end

       (_stuff,end) = break (== ',') rest'

   in  uncurry KW ans : listkws (trim $ softtail end)

-- so, in light of the above, readComma becomes superfluous? Nope: MapRowElement

readComma :: (Read n, Read a) => (n -> a -> f) -> String -> f
readComma f (break (== ',') -> (n,(_comma:list))) =
   f (read n) (read (trim list))

trim :: String -> String
trim [] = []
trim list@(h:t) | h == ' ' = trim t
                | otherwise = list

{--
data SpacyList a = SL [a]
   deriving (Eq, Ord, Show)

instance Read a => Read (SpacyList a) where
   readsPrec _ = xxx
--}

data MapRowElement = MRE Int [Keyword]
   deriving (Eq, Ord, Show)

tuple :: MapRowElement -> (Int, [Keyword])
tuple (MRE n list) = (n, list)

bracketed :: Char -> Char -> (String -> a) -> String -> [(a, String)]
bracketed match1 match2 readf [] = []
bracketed match1 match2 readf (s:tring) =
   if s == match1 && last tring == match2 then [(readf (init tring),"")] else []

instance Read MapRowElement where
   readsPrec _ = bracketed '[' ']' (readComma MRE)

-- From our MapRowElements we need to realize a map:

type KeywordMap = Map Int [Keyword]

rows2Map :: [MapRowElement] -> KeywordMap
rows2Map = Map.fromList . map tuple

-- NOW you can read in the file.

readKeywords, readCompressedKeywords :: FilePath -> IO KeywordMap
readKeywords = fmap (rows2Map . map read . lines) . readFile
readCompressedKeywords =
   fmap (rows2Map . map read . lines . BL.unpack . GZ.decompress) . BL.readFile

-- How many keywords does id 12 have? How many elements does the map have?

{--
>>> kwMap <- readKeywords (kwDir ++ "kw_index_file.txt")
>>> length kwMap
100
>>> length (kwMap Map.! 12)
30
--}
