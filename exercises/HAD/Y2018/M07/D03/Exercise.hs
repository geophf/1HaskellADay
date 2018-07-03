module Y2018.M07.D03.Exercise where

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

-- below imports available via 1HaskellADay git repository

import Data.Relation

import Graph.Query

import Y2018.M06.D27.Exercise

graphMeBAYBEE :: Codex -> IO ()
graphMeBAYBEE codex = undefined

{-- 
graphMeBAYBEE takes the codex you created in Y2018.M06.D27 and returns 
(in the real world) a graph database, it does this by converting the codex
to a set of Relation values.

>>> honk <- readMap (exDir ++ honkin)
>>> codex = mapping2Codex honk
--}

codex2Rels :: Codex -> [Relation Vert Arr Vert]
codex2Rels codex = undefined

-- of course, you neet the vert and arr types

data Vert = V Vertex
data Arr =  E Strength

-- both of which have to be defined as instances in the Relation domain

instance Node Vert where
   asNode vert = undefined

instance Edge Arr where
   asEdge arr = undefined

-- from this we should be able the Cyph the codex. What do you get?
