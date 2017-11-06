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
   readList = bracketed '[' ']' listkws  -- in theory should work, so long as no
                                         -- SQS has a ']'-character

forceOrder :: Read n => String -> [((n, SingleQuotedString), String)]

-- okay, so keyword tuples are coming at us either (strength, sqs) OR (sqs, strength)
-- WTF? Okay, whatever. So we just ask our parser to deal with it.

forceOrder str@(_openParen:first:rest) | first == '\'' =
          -- inverted structure: (sqs, strength)
   let (str1, _quot:_comma:numParen) = break (== '\'') rest
       (num,_closeParen:restRest) = break (== ')') (trim numParen) in
   [((read num, SQS str1), restRest)]
                            | otherwise     = -- (n, 'quot') order
   let (num, _comma:str1) = break (== ',') (tail str)
       (str2, _quot1:_closeParen:restRest) = break (== '\'') (tail (trim str1)) in
   [((read num, SQS str2), restRest)]

listkws :: String -> [Keyword]
listkws [] = []
listkws listElts = 
   let [(ans, rest')] = forceOrder listElts

-- now, rest' may have a comma, or may be the end

       (_stuff,end) = break (== ',') rest'

   in  uncurry KW ans : listkws (trim $ softtail end)

trim :: String -> String
trim [] = []
trim list@(h:t) | h == ' ' = trim t
                | otherwise = list

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

readComma :: (Read n, Read a) => (n -> a -> f) -> String -> f
readComma f (break (== ',') -> (n,(_comma:list))) =
   f (read n) (read (trim list))

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
