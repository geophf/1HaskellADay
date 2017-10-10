{-# LANGUAGE OverloadedStrings #-}

module Y2017.M09.D22.Solution where

import qualified Codec.Compression.GZip as GZ
import Control.Arrow ((&&&))
import Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Network.HTTP.Conduit

-- below import available via 1HaskellADay git reposiory

import Control.DList

{--
Okay, back to something 'simple' like document processing.

We have a gzipped archive of a set of NYT (New York Times) articles as 
unstructured plain text and we need to

1) find out the underlying (implied) structure, then
2) reify Article values from the unstructured input

First problem, and it's an easy one (relatively speaking).

Read in the compressed archive and realize a set of articles from that archive.

Here's why I say it's an easy problem:
--}

data Article = Art { artId :: Integer, rawText :: ByteString }
   deriving (Eq, Ord, Show)

-- so an article is just a BLOB of data, still unstructured

scanArticles :: ByteString -> [Article]
scanArticles = eachArticle emptyDL 1 . tail . BL.lines

eachArticle :: DList ByteString -> Integer -> [ByteString] -> [Article]
eachArticle dl x [] = let a@(Art _ txt) = reifyArt x dl in
   if BL.length txt > 0 then [a] else []   -- added if to delete empty articles
eachArticle dl x (h:t) = cont (isLinebreak h) h dl x t

cont :: Bool -> ByteString -> DList ByteString -> Integer -> [ByteString] -> [Article]
cont True _ dl x t = reifyArt x dl : eachArticle emptyDL (succ x) t
cont False h dl x t = eachArticle (dl <| h) x t

reifyArt :: Integer -> DList ByteString -> Article
reifyArt x dl = Art x (BL.unlines $ dlToList dl)

isLinebreak :: ByteString -> Bool
isLinebreak =
     (== "_______________________________________________________")
   . BL.take 55

-- you should get 11 articles from your scan of 

dir :: FilePath
dir = "Y2017/M09/D22/"

arts :: FilePath
arts = "NYT_articles.txt.gz"

-- and go!

{--
>>> articles <- scanArticles . GZ.decompress <$> BL.readFile (dir ++ arts)
>>> length articles
12

Okay, twelve, then.

>>> (Art idx art) = head articles
>>> print idx >> BL.putStrLn (BL.take 80 art)
1

Document 1 of 1000

Mario M. Cuomo, 1932-2015

Author: Nagourney, Adam

Publica

For the 12th article:
>>> (Art idx art) = last articles
>>> print idx >> BL.putStrLn (BL.take 80 art)
12

but:

>>> BL.length art
0

AHA! So eleven articles, only! Let's add logic to drop empty articles.

... after adding the if-guard to eachArticle clause

>>> articles <- scanArticles . GZ.decompress <$> BL.readFile (dir ++ arts)
>>> length articles
11

BOOM!
--}

{-- BONUS -----------------------------------------------------------------

convert the list of articles into JSON
--}

instance ToJSON Article where
  toJSON (Art idx txt) = object ["idx" .= idx, "txt" .= BL.unpack txt]

{--
>>> BL.putStrLn $ encode (last articles)
{"idx":11,
 "txt":"\nDocument 11 of 1000\n\nPoetic Voice Wrapped Tight in Its Shifting Politics\n\nAuthor:..."}

EASY-PEASY!
--}

{-- BONUS-BONUS -----------------------------------------------------------

Define this function. Just for the love of it
--}

articleTextById :: [Article] -> Map Integer ByteString
articleTextById = Map.fromList . map (artId &&& rawText)

{--
>>> mappage = articleTextById articles
>>> length mappage 
11
>>> mapM_ (\(k,v) -> print k >> BL.putStrLn (BL.take 60 v)) (Map.toList mappage)
1

Document 1 of 1000

Mario M. Cuomo, 1932-2015

Author: Nago
2

Document 2 of 1000

Mario Cuomo, 82, Dies; Three-Term Gover
3

Document 3 of 1000

Radical Islam, Nihilist Rage

Author: M
4

Document 4 of 1000

Challenging Critics of Religion Who Bla
5

Document 5 of 1000

Mario Cuomo Spoke Willingly of His Reli
6

Document 6 of 1000

Experts Say That Battle on Keystone Pip
7

Document 7 of 1000

Emotion Mixes With Politics as 4 Killed
8

Document 8 of 1000

Pope Says Sri Lanka Must Confront Truth
9

Document 9 of 1000

A Multiethnic Movement Emerges in Guyan
10

Document 10 of 1000

In Targeting Ex-Premier, Thai Junta Ta
11

Document 11 of 1000

Poetic Voice Wrapped Tight in Its Shif
--}
