module Y2017.M11.D16.Exercise where

{--
Yesterday we used keywords, rended from key-phrases, and were able to narrow a
search through a database of articles. Great!

Well, some 'narrowed' searches returned 2000+ articles, or 778 articles. That's
still a lot of ground to cover, and the search results didn't rank the returned
results. How do we know what articles were most relevant? And how do we put
those articles first?

The problem with (my) yesterday's solution was that I got the keywords rendered
from the key-phrases, but I didn't use the key-phrase-strength associated with
the source article.

Today's Haskell problem is to associate those keywords back to their source
key-phrases so that the relevance of an article can be ranked, using the key-
phrase strength as the determinant.

That is to say:

1. for each keyword, k, provide a (multi-)mapping from k to its key-phrases.

Then:

2. for each recommended article, a, provide a strength that is the sum of its
   key-phrases associated with the keywords use to narrow the search to this
   article.

Then:

3. Return the ranked article set from a keyword-search, most relevant article
   first.

Okay, okay, okay. Timeout. I mean, it's doable to, on the fly, compute these
mappings, but it seems like an awfully lot of work that we're going to do over
and over, each time we do an article-search.

Let's rethink this.

Okay.

So, let's do this computation once-...ish. That is, each time we add a 
keyphrase-set to the database, we add the associated and joined keywords.

Which means we have keywords added from before. Which means: 

Enter the MemoizingTable!

But not for now. Let's just do this thing, then upload the results after
we do have a working set of results with to work.
--}

-- 1. We have a KeyWord -> Set ArticleId mapping. Provide a 
--    KeyWord -> Set Keyphrase mapping

{--

Here was a rant that I had that I went back and fixed because I got annoyed.

Hysterical Raisins annoy me. "Oh, we've always done it that way."

Oh, have you?

-- Oh, and KeyWord and Keyword look bloody well the same, why? Because
-- key-phrases are called keywords for 'hysterical raisins.' That is to say:
-- somewhere along the line, somebody started classifying articles by 
-- key-phrases instead of just by keywords and never updated the terminology.
--}

import Data.Map (Map)
import Data.Set (Set)

-- below imports available via 1HaskellADay git repository

import Y2017.M11.D03.Exercise  -- Keyphrase and its ilk
import Y2017.M11.D13.Exercise  -- KeyWord and its ilk

type KeyWordKeyphrases = Map KeyWord KeyphraseMap

kw2kw :: KeyphraseMap -> IndexedArticles -> KeyWordKeyphrases
kw2kw kwmap keywords = undefined

-- maps the KeyWord values to sets of keyphrased and their associated articles
-- include ONLY the keyphrases associated with the keyword mapped.

-- Now, with the results of yesterday:

-- 1. what are the first five articles returned from yesterday's refined search?

-- Now, rank articles by associated strength, that is to say, accumulate and add
-- the strengths of the keyphrases for the articles for the keywords searched

ranked :: KeyWordKeyphrases -> Set ArticleId -> [KeyWord] -> [ArticleId]
ranked kwjoin arts keys = undefined

-- 2. What, now are the top five (that is to say: the top five highest ranked)
--    articles?
