{-# LANGUAGE OverloadedStrings #-}

module Y2017.M09.D15.Solution where

import Control.Arrow ((&&&))
import Control.Monad.State
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (toLower, isAlpha)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio ((%))

-- below imports available via 1HaskellADay git repository

import Y2017.M09.D08.Solution (composePath, articlesAt, Directory)
import Y2017.M09.D13.Solution (articlesDir)

{--
Today we are going to build a KEA, a Keyword Extraction Algorithm, a simple one,
but one, none-the-less. Our approach will be thus:

take an article, 
rend it into individual words,
get the total word count for that article
get the word count for each unique word in the article
compute the strength of each word of the article
--}

data KeyWord = KW { kwId, count :: Int, strength :: Rational }
   deriving (Eq, Show)

-- a keyword has an id, a count in the document, and its relative strength

type Dictionary = Map String Int

-- a dictionary is a list of keywords and their ids

kea :: Int -> Dictionary -> ByteString -> WordContext
kea idx dict file = 

-- this let-clause counts the valid words inline as it sanitizes them
   let (cnt, wrds) =
           foldr (\wrd (c0, w) ->
                case sanitize wrd of
                     [] -> (c0, w)
                     w1 -> (succ c0, w1:w)) (0, []) (words (BL.unpack file))
   in  execState (mapM_ (kea' cnt) wrds) (WC dict Map.empty idx)

-- note that I sanitize then I update the dictionaries. This is a two-pass
-- approach. Can this be redefined as one-pass with a cnt promise?

-- kea scans a file, assigning a count and weight to each keyword. If the
-- keyword isn't in the dictionary, create a new entry with that keyword

sanitize :: String -> String
sanitize = map toLower . filter isAlpha

{--
>>> sanitize "Hello,"
"hello"
--}

-- A helper function might be ... well, helpful:

data WordContext =
   WC { dict :: Dictionary, kws :: Map Int KeyWord, index :: Int }
      deriving Show

kea' :: Integer -> String -> State WordContext ()

{-- we have two approaches:

1. the word is in the dictionary. Look up the keyword by index and update it.
2. the word is not in the dictionary. Add it to the dictionary and keyword list.

--}

kea' wordCount word =
   get >>= put . newWC wordCount word . (Map.lookup word . dict &&& id)

newWC :: Integer -> String -> (Maybe Int, WordContext) -> WordContext
newWC cnt wrd (Just idx, WC dic kws x) =

   -- case 1a and 1b: word in dictionary. update keyword set

   WC dic (Map.insert idx (uncurry (KW idx)
          ((id &&& (% cnt) . fromIntegral)
           (case Map.lookup idx kws of
                   Just (KW y c _) -> succ c        -- already in keyword set
                   Nothing         -> 1))) kws) x   -- not in keyword set

newWC cnt wrd (Nothing, WC dic kws x) =

   -- case 2: word not in dictionary. Add it to both dictionaries. Update count.

   WC (Map.insert wrd x dic) (Map.insert x (KW x 1 (1 % cnt)) kws) (succ x)

-- given a dictionary and a set of keywords, update the dictionary with a new
-- word (and add that word's key to the keyword index), or update the keyword
-- index if the word is already in the dictionary.

{--
>>> execState (kea' 12 "the") (WC Map.empty Map.empty 0)
WC {dict = fromList [("the",0)],
     kws = fromList [(0,KW {kwId = 0, count = 1, strength = 1 % 12})],
   index = 1}

... and then ...
>>> execState (kea' 12 "the") it
WC {dict = fromList [("the",0)],
     kws = fromList [(0,KW {kwId = 0, count = 2, strength = 1 % 6})],
   index = 1}

... and then ...
>>> execState (kea' 12 "apple") it
WC {dict = fromList [("apple",1),("the",0)], 
     kws = fromList [(0,KW {kwId = 0, count = 2, strength = 1 % 6}),
                     (1,KW {kwId = 1, count = 1, strength = 1 % 12})],
   index = 2}
--}

-- when you've defined the above, what is the keyword set of:

testFile :: FilePath
testFile = composePath articlesDir "AP900327-0094.txt"

{--
>>> kea 0 Map.empty <$> BL.readFile testFile
WC {dict = fromList [("a",37),("about",25),("after",10),...,("with",23)],
     kws = fromList [(0,KW {kwId = 0, count = 11, strength = 11 % 139}),...],
   index = 91}

--}

{-- BONUS -----------------------------------------------------------------

That's all fine and dandy for just one article, but what happens when you
analyze a set of articles? Compute the set of keywords for all the articles
in articlesDir
--}

keywordsAllArticles :: Directory -> IO (Dictionary, Map String (Map Int KeyWord))
keywordsAllArticles dir =
    fmap snd (
         articlesAt "" dir >>=
           foldM (\(idx, (dict, mappo)) filename ->
                 kea idx dict <$> BL.readFile filename >>= \(WC d1 m i') ->
                 return (i', (d1, Map.insert filename m mappo)))
                (0, (Map.empty, Map.empty)))

{--
>>> keywordsAllArticles articlesDir 
(fromList [("a",35),("abc",47),...,("york",332),("you",427),("youve",433)],
 fromList [("Y2017/M09/D08/articles/b//AP900327-0002.txt",
            fromList [(0,KW {kwId = 0, count = 1, strength = 1 % 159}),...]),...,
           ("Y2017/M09/D08/articles/b//AP900402-0100.txt",
            fromList [(1,KW {kwId = 1, count = 4, strength = 4 % 699}),...])])
--}
