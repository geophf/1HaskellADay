module Y2017.M10.D30.Exercise where

{--
Hello! Welcome back! Happy Monday!

Of course, when you're working in a start-up, the days can run together in a
blur, I've found.

So, we've been working with the NYT archive for more than a month now. Did you
notice anything about the data? I have:
--}

import Control.Monad
import Data.Map (Map)

-- below imports available via 1HaskellADay git repository

import Y2017.M10.D23.Exercise -- for Article structure
import Y2017.M10.D24.Exercise (articlesFromFile)

sampleText :: String
sampleText = "GALVESTON, Tex. \226\128\148 Adolfo Guerra, a landscaper"

-- The thing is, if you look at this in a text editor (worth its salt) you see:

-- GALVESTON, Tex. — Adolfo Guerra, a landscaper

{-- 
So, we need:

1. to identify the documents that have special characters
2. the context of where these special characters occur
3. once marked, replace the special characters with a simple ASCII equivalent

Let's do it.
--}

data Context = Ctx { ctxid :: Integer, spcCharCtx :: SpecialCharacterContext }
   deriving (Eq, Show)

type SpecialCharacterContext = Map SpecialChars [Line]
type SpecialChars = String
type Line = String

identify :: MonadPlus m => Article -> m Context
identify art = undefined

-- identifies the places in the text where there are special characters and
-- shows these characters in context ... now, since the full text is one 
-- continuous line, getting a line is rather fun, isn't it? How would you
-- form the words around the special characters? Also, how do you extract
-- the special characters from the full text body?

type BodyContext = [String]

extractSpecialChars :: BodyContext -> [(SpecialChars, Line)]
extractSpecialChars ctx = undefined
 
-- sees if we're at a set of special characters then accumulates a context
-- around those characters. N.b.: special characters can occur as their own
-- word, or in a word at the beginning, or within, or at the end of a word,
-- e.g. ("Don't..." he said) if the quotes and apostrophe and ellipse are 
-- special characters then you have an example of special characters all around
-- the word 'don't' including the (') within the word.

{--
So you should get the following:

>>> extractSpecialChars (words sampleText)
[("\226\128\148","GALVESTON, Tex. \226\128\148 Adolfo Guerra,")]
--}

-- With the set of articles from Y2017/M10/D24/hurricanes.json.gz
-- What are the special characters, in which articles, and in what contexts?

-- Tomorrow we'll look at building a replacement dictionary
