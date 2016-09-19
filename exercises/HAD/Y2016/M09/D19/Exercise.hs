module Y2016.M09.D19.Exercise where

-- Below imports available in 1HaskellADay git repository

import Data.BlockChain.Block.Types
import Data.Graphics.SVG
import Data.Monetary.BitCoin
import Data.Relation
import Data.Tree.Merkle

import Graph.JSON.Cypher

{--
Okay, from a couple of weeks ago, we are now creating Merkle trees that are
balanced, and we even fetched blocks from the block chain and created our own
Merkle tree.

GREAT!

The output, however, is rather on the heavy-side, from a massive wall of text
perspective. For the human eye, this leaves much to be desired.

Let's fix that.
 
Today, we are going to create a Merkle tree from some source data and then
visualize that Merkle tree in how-so you see fit.
--}

-- 1. We've already proved that we can fetch real blocks from the block chain
--    given a set of hashes, but let's simplify things a bit here. Let's reduce
--    a block to a simplified transaction. A transfers x BTC to B

type Name = String

data Person = P Hash Name deriving (Eq, Show)

peeps :: [Person]
peeps = undefined

-- define the function peeps such that it returns 10 people in howsoever you
-- wish to generate these people

data Xaction = X Person BitCoin Person deriving (Eq, Show)

xactions :: [Xaction]
xactions = undefined

-- Now, have xactions return 25 transcations, Person A transfers x BTC to
-- Person B. Again, define these transactions howso you wish

viewmerk :: [Xaction] -> IO ()
viewmerk = undefined

-- Finally, put all those transactions into a Merkle tree, and then output
-- some visualization of that Merkle tree. Use whatever visualization tool
-- you prefer: SVG, Relations-as-Graph, or the Haskell diagram tool-suite.

-- Or come up with your own visualization.

-- We'll be looking at comparing, copying, and perhaps voting on Merkle trees
-- throughout the rest of this week.
