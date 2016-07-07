module RID.Graph where

{-- a solution to the problem posted at http://lpaste.net/7822865785260343296

-- 2015-11-11: updated to graph the entire RID (not just PRIMARY cognition)

Okay, yesterday we showed part of the RID as a tree-structure. That's fine,
the RID is tree(-like).

The problem here came up with the question: when I found the pathway to BACON,
I did find BACON (YAY!), but I had to scan the entire tree to do it, because
of the operational semantics of the declared tree. And there's (short)cuts to
this (à la Prolog (!) (pronounced 'cut')), but we still have to do a lot of
traversals to find what we already know, and then backtrack to accumulate
the path.

That seems wasteful. Why don't I just index from BACON and then link 'up' to
its ancestral categories?

Why not, indeed?

But how do we do that?

ONE way of doing that is that any tree is a (sub-)graph, so if we map the
tree into a graph, we can start from any vertex and find the links into it
and thereby build the pathway starting from the node (vertex) BACON.

Sweet.

Let's do that today.

Either from the raw data at http://lpaste.net/raw/8106042088711258112 or
from the tree-structure constructed from RID.Tree, realize a Data.Graph.Graph
and answer the below questions.
--}

import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Graph
import Data.List (partition, isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Tree

import RID.Tree

rid2graph :: RID -> (Graph, Vertex -> (Kinds, String, [String]),
                     String -> Maybe Vertex)
rid2graph = graphFromEdges . cogs2assocs . Map.toList . cogs

cogs2assocs :: [(Cognition, Map String Category)] -> [(Kinds, String, [String])]
cogs2assocs = map (\(cog, cats) -> (Cog cog, show cog, Map.keys cats))
          &&& concatMap (mapAllValues . snd)
          >>> uncurry (++)

{-- 
*RID.Graph> readinRID "RID" ~> rid
*Main Control.Monad> let (graph, fromVert, keyf) = rid2graph rid
*Main Control.Monad> graph ~> 
array (0,3197) [(0,[]),(1,[]),(2,[]),(3,[]),(4,[]),...]

Looks about right as there are ~3200 words in the rid
--}

mapAllValues :: Map String Category -> [(Kinds, String, [String])]
mapAllValues = concatMap catcats . Map.toList

catcats :: (String, Category) -> [(Kinds, String, [String])]
catcats (key, Cat cat kids) =
   (\chi -> (Catg key, cat, map (nameof . snd) chi) : concatMap catcats chi)
   (Map.toList kids)
catcats (key, WordSet ws wrds) =
   (\elts -> (Catg key, ws, map show elts) : map selfword elts) (Map.elems wrds)

selfword :: RootWord -> (Kinds, String, [String])
selfword s = (Rwrd s, show s,[])

-- What goes with BACON? FIGS, obviously. (No, really: preserved figs for
-- breakfast? Killer!) What is the Path to FIGS?

{--
*RID.Graph> keyf "FIGS (1)" ~> Just 1051
*RID.Graph> vertf 1051 ~> (Rwrd FIGS (1),"FIGS (1)",[])
--}

pathing :: (Graph, Vertex -> (Kinds, String, [String]), String -> Maybe Vertex)
        -> WordFragment -> Maybe Path
pathing (gr, vertf, keyf) =
   liftM (pathing' vertf (dfs (transposeG gr) . return)) . keyf

-- ... and to help pathing along ...:

pathing' :: (Vertex -> (Kinds, String, [String])) -> (Vertex -> Forest Vertex)
         -> Vertex -> Path
pathing' vertf pathf = reverse . map (view _2 . vertf) . concatMap flatten . pathf

{--
*Main> pathing (gr,vertf, keyf) "FIGS (1)" ~>
Just ["PRIMARY","NEED","ORALITY","FIGS (1)"]
*RID.Graph> pathing' vertf (dfs (transposeG gr) . return) 1051 ~>
["PRIMARY","NEED","ORALITY","FIGS (1)"]
--}

-- I want your SEX! What are the vertices, both parent and children, associated
-- with the SEX-vertex?

neighbors :: (Graph, String -> Maybe Vertex) -> String -> Maybe [Edge]
neighbors (gr, keyf) key =
   liftM (flip filter (edges gr) . (\v (a,b) -> v == a || v == b)) $ keyf key

{--
*Main> neighbors (gr, keyf) "SEX" ~>
Just [(1052,1408),(1408,7),(1408,8),(1408,26),... 116 elements total]

Of course "SEX" is vertex 1408!

*Main> keyf "SEX" ~> Just 1408

See?
--}

-- HINT: The type-variable pathingmap is intentionally ambiguous. What functions
-- in Data.Graph help to build a graph from input sources and then provides
-- a pathing function to the vertices?

-- 2. What are all the word-roots of the RID?

wordRoots :: RID -> Map RootWord Path

{-- BUNCHES away to go about this:
1. Craft two algorithms: one for Primary and then the other for the others
2. Generic flattening of trees, collecting the leaf-nodes
3. Create a lookup table while building the RID, a reversed tree, as it were.
4. Use the Data.Graph builder and then locate all vertices with outbound-only
   edges
5. ... your thoughts?
--}

wordRoots = graphRoots . rid2graph

graphRoots :: (Graph, Vertex -> (Kinds, String, [String]),
               String -> Maybe Vertex) -> Map RootWord Path
graphRoots (gr, vertf, _) =
   let pathf = dfs (transposeG gr) . return in
   Map.fromList
   . mapMaybe (\v -> liftM (\wrd -> (wrd, pathing' vertf pathf v))
                           (rw . view _1 $ vertf v))
   $ vertices gr

rw :: Kinds -> Maybe RootWord
rw (Rwrd r) = return r
rw _        = Nothing

-- okay, so I went with the RID-as-Graph solution

-- *Main> length (wordRoots rid) ~> 2808

-- 3. Of the root words, how many of them are whole words, and how many
--    are words that allow additional letters to be appended to them?

type WholeWordsCount = Int
type WithWildcardsCount = Int

kindsOfWords :: RID -> (WithWildcardsCount, WholeWordsCount)
kindsOfWords = join (***) length . partition wildcard . Map.keys . wordRoots

-- *Main> kindsOfWords rid ~> (2387,421)

{-- BONUS -------------------------------------------------------------------

There appear to be some redundancies in the RID for word-matching.

For example, there are three RootWord values: BITE*, BITE, BITES

They all could be captured by BITE* if the sole purpose is to capture words
in the PRIMARY->NEED->ORALITY category and subcategory.

Identify all redundancies.
--}

redundancies :: RID -> [[RootWord]]
redundancies = filter ((>1) . length) . r' . Map.keys . wordRoots
   where r' [] = []
         r' list@(RW wrd True:t) =
            (partition (\(RW x _) -> isPrefixOf wrd x)
             >>> second r' >>> uncurry (:)) list
         r' (RW wrd False:t) = r' t

{--
*Main> redundancies rid
[[ABUS* (1),ABUSIV* (1)],[ACCEPT* (1),ACCEPTANC* (1)],...]
*Main> length it ~> 319

This bonus questions goes away with the redefinition of Eq RootWord that
automa(gi|ti)cally eliminates redundancies

*RID.Graph> redundancies rid ~> []

WOOT!
--}
