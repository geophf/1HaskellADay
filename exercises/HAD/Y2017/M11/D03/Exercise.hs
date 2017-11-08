module Y2017.M11.D03.Exercise where

{--
So, just got a metric tonne of new information from the NYT article archive.
Last Thursday (Y2017.M10.D26) we used artificial-artificial intelligence to
analyze a set of articles for topicality. Today, we've got results from key
phrase extraction algorithms to identify the topicality for sets of articles.

The format, including all its beautious noise is here as kw_index_file.txt
--}

import Data.Map (Map)

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

type Strength = Double
data Keyphrase = KW { strength :: Strength, keyphrase :: SingleQuotedString }
   deriving (Eq, Ord, Show)

instance Read Keyphrase where
   readsPrec = undefined
   readList = undefined  -- careful!

sampleKeyphrase :: String
sampleKeyphrase = "(9.0, 'subject line .)')"

sampleKeyphraseList :: String
sampleKeyphraseList = "[(12.25, 'state department would say'), (12.236813186813185, 'pifer said american diplomats')]"

data MapRowElement = MRE Integer [Keyphrase]
   deriving (Eq, Ord, Show)

instance Read MapRowElement where
   readsPrec = undefined

data SingleQuotedString = SQS String
   deriving (Eq, Ord, Show)

instance Read SingleQuotedString where
   readsPrec = undefined

-- From our MapRowElements we need to realize a map:

type KeyphraseMap = Map Integer [Keyphrase]

rows2Map :: [MapRowElement] -> KeyphraseMap
rows2Map rows = undefined

-- NOW you can read in the file.

readKeyphrases :: FilePath -> IO KeyphraseMap
readKeyphrases kwFile = undefined

-- How many keywords does id 12 have? How many elements does the map have?
