module Y2016.M12.D08.Exercise where

-- below import available from 1HaskellADay git repository

import Data.Matrix

{--
Today's Haskell exercise comes all the way from Smyrna.

Some 2000 or 2500 years ago, somebody scrawled this on a wall in the market:

Μ Η Λ Ο Ν
Η Δ Ο Ν Η
Λ Ο Γ Ο Σ
Ο Ν Ο Μ Α
Ν Η Σ Α Σ

So, I know ΛΟΓΟΣ, is 'logos'/'word,' but what are the other words?

Today's exercise is to construct word squares.

Construct one or more word squares of size n, that is to say, given n, the
length of the word square, choose words from whichever language you like and
construct a word square, that is to say: each word is read horizontally and
vertically.

You can use, e.g. /usr/share/dict/words (on my unix-y laptop, that's where
my dictionary is, YMMV) or you can use these words here:

http://lpaste.net/112882

Or you can use the proteins, starting here:

https://en.wikipedia.org/wiki/List_of_proteins

(ooh, tricky, geophf!)

Or you can use the list of Java keywords here

http://docs.oracle.com/javase/tutorial/java/nutsandbolts/_keywords.html

... okay, don't puke. I'm just saying.

Or whatever you like.
--}

wordSquare :: Int -> [String] -> [Matrix Char]
wordSquare n lexicon = undefined
