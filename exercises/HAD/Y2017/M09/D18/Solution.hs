{-# LANGUAGE OverloadedStrings #-}

module Y2017.M09.D18.Solution where

import Control.Monad (foldM)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio ((%))

-- below import available via 1HaskellADay git repository

import Data.Bag

import Y2017.M09.D08.Solution (articlesAt, exerciseDir, Directory)
import Y2017.M09.D15.Solution (sanitize)

{--
Today we will take a dictionary of 'things' and determine their relative
'strength.'

Say you have a mapping of words and their occurrences in a document (e.g.: the
solution to Y2017.M09.D15.Exercise). With that mapping compute a new mapping:
--}

type Strength a = Map a Rational

wordStrength :: Map String Int -> Strength String
wordStrength wordcounts =
   let dem = fromIntegral (sum (Map.elems wordcounts)) in
   Map.map (strength dem . fromIntegral) wordcounts

-- wordStrength computes the total number of words in a document from the
-- occurrence count and outputs the 'strength' of each word where:

strength :: Integer -> Integer -> Rational
strength totalWords wordCount = wordCount % totalWords

{--
>>> wordStrength (Map.fromList [("the", 24), ("a", 12), ("chocolate",3)])
fromList [("a",4 % 13),("chocolate",1 % 13),("the",8 % 13)]
--}

-- compute the strengths of the words in the documents in
-- Y2017/M09/D08/articles/n/

-- First we need a word counter.

wc :: ByteString -> Map String Int
wc = asMap
   . foldr (\wrd bag -> case sanitize wrd of
                              [] -> bag
                              w  -> add w bag) emptyBag . words . BL.unpack

{--
>>> wc <$> BL.readFile ("Y2017/M09/D08/articles/n/explicit-geometry-in-low-dimensions.txt")
fromList [("a",1),("abstract",1),("all",1),("an",1),("and",2),...,("with",1),("work",2)]

>>> wordStrength it
fromList [("a",1 % 165),("abstract",1 % 165),...,("work",2 % 165)]
--}

-- and then we have our strengthifier:

strengthifier :: Directory -> IO (Map FilePath (Strength String))
strengthifier dir =
   articlesAt "" dir >>=
   foldM (\m filename ->
                fmap (wordStrength . wc) (BL.readFile filename) >>= \dict ->
                return (Map.insert filename dict m)) Map.empty

{--
>>> mapM_ ((\(title, top5) -> print title *> print top5) . second take5) . take5 
                 <$> strengthifier (exerciseDir ++ "/n") 
"Y2017/M09/D08/articles//n//estrogen-receptor-beta-splice-variants-and-brain-development.txt"
[("a",1 % 54),("action",1 % 216),("acts",1 % 216),("affect",1 % 216),("alpha",1 % 216)]
"Y2017/M09/D08/articles//n//evolution-of-phoresy-and-feeding-associations-in-the-mite-order-parasitiformes.txt"
[("a",4 % 163),("about",1 % 326),("abstract",1 % 326),("allowing",1 % 326),("alpha",1 % 326)]
"Y2017/M09/D08/articles//n//experiments-for-integrating-bme-concepts-into-the-ece-curriculum.txt"
[("a",13 % 314),("accredited",1 % 314),("achieved",1 % 314),("acquainting",1 % 314),("along",1 % 314)]
"Y2017/M09/D08/articles//n//experiments-on-uniform-shear-at-high-reynolds-number.txt"
[("abstract",1 % 121),("and",4 % 121),("approaches",1 % 121),("are",4 % 121),("as",1 % 121)]
"Y2017/M09/D08/articles//n//explicit-geometry-in-low-dimensions.txt"
[("a",1 % 165),("abstract",1 % 165),("all",1 % 165),("an",1 % 165),("and",2 % 165)]
--}
