module Y2017.M11.D13.Exercise where

-- Okay, we've got keywords from

import Y2017.M11.D03.Exercise

{--
Now, let's use them. Today we're going to scan into our keyword set and find
articles that contain these keywords.

There's a catch: these keywords are actually key-phrases as well as keywords. 
What if a key-phrase has the sought word? Are we going to rend the key-phrase 
into words and scan that? Are we going to front-load, then memoize, that work?

Let's see.

Using the work from before (kwDir, readKeyphrases), get the keywords from 
Y2017/M11/D03/

Okay, now create a search-algorithm. It takes a set of keys as and-constraints, 
and returns the set of article ids matching those keyword constraints.

But before you do that, you probably want to do an inversion: the 
KeyphraseMap-type is of the form Map ArticleId [Keyphrase] but we want the 
inverted mapping. Let's do that today.
--}

import Data.Map (Map)
import Data.Set (Set)

type KeyWord = String -- denotes a sigle word
type ArticleId = Integer

type IndexedArticles = Map KeyWord (Set ArticleId)

searchKeymap :: KeyphraseMap -> IndexedArticles
searchKeymap kwm = undefined

{-- 
The above function is the goal, but before we do that, we want to break up the 
key-phrases in the keyword map into individual keywords, and then map the 
article Id for those keywords. Is there going to be redundancy? Yes, but then, 
that's why there're sets on both ends of the mapping: the map's keys are a set, 
and each map key points to a set. Let's see how this goes.

So, for each entry in the keyword map, we want to extract the words of the 
Keyphrase values then add all those words the article Id to their respective 
sets.
--}

invert :: [Keyphrase] -> Set KeyWord
invert keywords = undefined
   -- hint: What function breaks up a string into a list of words?

-- Now that we have the words we add the article Id to the sets in the map.
-- If there is no entry for key word, we initialize it with a singleton of 
-- article id

addKeyWords :: ArticleId -> Set KeyWord -> IndexedArticles -> IndexedArticles
addKeyWords artid keywords arts = undefined

{--
Do that iteratively over the entire keyword set and you will have your indexed 
articles.

Now. How many unique KeyWords do you have? What keyword has the most articles? 
What keyword has the second most number of articles? What is the intersection 
of those two keywords?

Put another way: given a list of keywords, find the articles that intersect all 
those keywords.
--}

refineSearch :: IndexedArticles -> [KeyWord] -> Set ArticleId
refineSearch arts keywords = undefined

-- We'll look tomorrow at incorporating the metadata of the articles (that we 
-- get from the article Ids), such as publish_dt, to narrow the search further.
