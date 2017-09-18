module Y2017.M09.D19.Exercise where

import Data.Map (Map)

-- below import available via 1HaskellADay git repository

import Y2017.M09.D15.Exercise

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
--}

lookupWord :: Dictionary -> KeyWord -> Maybe String
lookupWord dict kw = undefined

-- given a mapping (string -> int) and a keyword (declared in Y2017.M09.D15)
-- return the word associated with the keyword encoding.

-- Now from the article repository that you've encoded into keywords, what
-- are the top five words used in that article repository?

top5Words :: Dictionary -> Map Int KeyWord -> [String]
top5Words dict kws = undefined
