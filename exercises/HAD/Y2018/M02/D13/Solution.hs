module Y2018.M02.D13.Solution where

{--
Okay, SOME of the articles from yesterday's problem MAY have unicode, and that
plays havok with plain-old String values.

Of course, the solution to that is to use Text values, instead, but will we?

Nooooooooooooo!

Of course not! Instead, we're going to remove any special characters from
the article texts and deal, instead, with just plain old ASCII.
--}

import Codec.Compression.GZip as GZ

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (ord)
import qualified Data.Map as Map

-- below import available via 1HaskellADay git repository

import Y2018.M02.D12.Solution

asciify :: Article -> Article
asciify art = art { text = map asc (text art) }

asc :: Char -> Char
asc c = if ord c < 128 then c else ' '

-- how many articles from yesterday's data set changed?

{-- 
>>> articles <- json2Arts "Y2018/M02/D12/arts.json.gz"
>>> old = arts articles 
>>> length old
26
>>> new = map asciify old
>>> diffs = zipWith (/=) old new
>>> map fst (filter snd (zip [1..] diffs))
[1,2,3,6,7,9,11,12,13,14,15,16,17,18,19,20,22,23,24,25,26]
>>> length it
21

21 out of 26 articles had non-ASCII characters.
--}
