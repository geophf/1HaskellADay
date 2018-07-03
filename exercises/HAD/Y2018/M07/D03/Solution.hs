module Y2018.M07.D03.Solution where

{--
Yesterday, we translated JSON that was a mapping of String -> String to a
Codex that was a mapping Vertex -> Relations

where Relations was Map Vertex (Vertex, Strength) and we learned that the
keys of the relations were really just indices of the arrayed (Vertex,Strength)
pairing.

Our PhD comfirmed this fact, and said he did it this way because, and I quote:

"I'm an idiot."

An honest PhD. What did I do to deserve this honor today?

So, today, let's upload the codex to a graph database. I'm partial to neo4j,
but you can choose any of them, even d3js or the sigma graphing library or 
whatever works for you.

I'm easy.
--}

import Control.Arrow ((***))

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple (swap)

-- below imports available via 1HaskellADay git repository

import Data.Relation

import Graph.Query

import Y2018.M06.D27.Solution

graphMeBAYBEE :: Endpoint -> Codex -> IO String
graphMeBAYBEE endpt = cyphIt endpt . codex2Rels

{-- 
graphMeBAYBEE takes the codex you created in Y2018.M06.D27 and returns 
(in the real world) a graph database, it does this by converting the codex
to a set of Relation values.

>>> honk <- readMap (exDir ++ honkin)
>>> codex = mapping2Codex honk
--}

type CodRel = Relation Vert Arr Vert

codex2Rels :: Codex -> [CodRel]
codex2Rels = concatMap entries2Rels . Map.toList

entries2Rels :: (Vertex, Map Vertex (Vertex, Strength)) -> [CodRel]
entries2Rels (vert, rels) = e2r' vert (Map.elems rels)

-- We've established the map keys are redundant

e2r' :: Vertex -> [(Vertex,Strength)] -> [CodRel]
e2r' vert = map (uncurry (Rel (V vert)) . (swap . (V *** E)))

-- of course, you neet the vert and arr types

data Vert = V Vertex
data Arr =  E Strength

-- both of which have to be defined as instances in the Relation domain

instance Node Vert where
   asNode (V vert) = constr "V" [("idx",vert)]

instance Edge Arr where
   asEdge (E str) = constr "Strength" [("percent",str)]

-- from this we should be able the Cyph the codex. What do you get?

{--
>>> endpt <- graphEndpoint
>>> graphMeBAYBEE endpt codex

And there you go!

Nope. there you don't go. I keep getting the following timeout error:

>>> ans <- graphMeBAYBEE endpt codex
*** Exception: HttpExceptionRequest Request {
  host                 = "127.0.0.1"
...

So, let's limit the size of the codex:

>>> length codex
19999

And graph the smaller codex instead:

>>> ans <- graphMeBAYBEE endpt (Map.filterWithKey (\k a -> k < 500) codex)
(graph in graph database; image attached)
--}
