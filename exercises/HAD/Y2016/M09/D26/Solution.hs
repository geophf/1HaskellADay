module Y2016.M09.D26.Solution where

{--
We'll now look at copying and comparing Merkle Trees

The idea of the block chain and bitcoin is that it allows for a decentralized
currency. Everybody has their own copy of the block chain (which is a Merkle
tree), and transfers occur when the participants in the block chain concur
with the new tree with the new transaction.

That's great. But the block chain is 65 gigabytes, as of this writing. What,
then? Are we to disiminate 65 gigs to everyone with each transaction?

Hardly.

Here is the power of Merkle trees: the hash give an indication of equivalence.
If you and I have the same (root) hash, then we have the same Merkle tree
(with a very high degree of certainty). If you and I don't have the same hash
then actually, we only have half (of a half (of a half (of a half (of a...))))
of a problem.

Why?

The hash changes on only the affected branch, so the root hash changes, as it
depends on its left and right children's hash, but then only one child hash
differs between our trees. That child branch, left or right, is the one we
need to change. We keep 'cdr'ing down (as it were) until we find the one node
that differs in the entire tree, ... that is, if our trees differ by only one
node.

So, today, we'll just do some compares, warming up for copying later.
--}

-- Below modules availablee from 1HaskellADay git repository

import Control.Presentation (laxmi)
import Control.Logic.Frege (adjoin, (<<-))
import Data.BlockChain.Block.Graphs
import Data.Monetary.BitCoin
import Data.Relation
import Data.Tree.Merkle

import Graph.JSON.Cypher
import Graph.Query

-- Create a Merkle Tree using the below BitCoin values as leaves -----------

coins :: [BitCoin]
coins = map BTC [5.7, 0.01, 1.02, 9.3]

coins2Merk :: [BitCoin] -> MerkleTree BitCoin
coins2Merk = fromList . map mkleaf

-- *Y2016.M09.D26.Solution> coins2Merk coins ~> Merkle { root = Parent ... }

{-- 
Actually: create two trees, a and b, from the same set of coins.
Are a and b equivalent?

*Y2016.M09.D26.Solution> let a = coins2Merk coins
*Y2016.M09.D26.Solution> let b = coins2Merk coins
*Y2016.M09.D26.Solution> a == b ~> True

Now, insert BTC 7.7 to tree b.

*Y2016.M09.D26.Solution> let bb = insertLeaf (mkleaf (BTC 7.7)) b
*Y2016.M09.D26.Solution> bb == a ~> False
--}

instance Node BitCoin where
   asNode (BTC b) = "BTC { val: " ++ laxmi 2 b ++ " }"

-- with the above instance we can see the Merkle trees as Cypher-graphs.

-- return an ordered list of nodes of the Merkle tree that are different
-- between tree a and tree b

diffNodes :: MerkleTree a -> MerkleTree a -> [Branch a]
diffNodes = uncurry dn <<- curry (adjoin root)

-- of course, MerkleTrees differ in their branches so...

-- We really only care about parent branches, anything else is ... eh.

dn :: Branch a -> Branch a -> [Branch a]
dn p@(Parent h a b) (Parent h1 c d) =
   (if h == h1 then [] else [p]) ++ r a c ++ r b d -- !!! NOT breath-first!!!
      where r = uncurry dn <<- curry (adjoin branch)
dn p@(Parent _ _ _) _ = [p]   -- traversal stops, tree2 exhausted
dn p1 p2 = if hashID p1 == hashID p2 then [] else [p1]

{--
*Y2016.M09.D26.Solution> diffNodes a bb
[Parent {hashID = "96704128e696729e5b180036137..."}, ...]

GAH! Showing the whole branch show the whole tree until we finally descend to
the branches which have different or new leaf-nodes.

Let's just look at the different hashes then:

*Y2016.M09.D26.Solution> map hashID diffs
["96704128e696729e5b18003613799d7f63d3792874328136687278fe1e436822",
 "e257d17bffc8060ff6bba291859b6f796c0321866c6ce8970bae723f059e593f",
 "629d3f4e57e4db047018134d7b7bdfa4664169b57c3c4d98c6bdc623e483cc00"]

Okay, we see three nodes that are different. I show these differenceis by 
uoloading trees a and bb to a graph viewer (posted to twitter).
--}
