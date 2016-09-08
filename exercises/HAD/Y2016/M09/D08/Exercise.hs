module Y2016.M09.D08.Exercise where

import Data.BlockChain.Block.Summary
import Data.BlockChain.Block.Types
import Data.Tree.Merkle

{--
Okay, we've built up an understanding of what a block is in the block chain,
what is next is to build our own block chain.

Last week (or so) we created Merkle trees, which are balanced binary trees
which hash their data. But what we did not do is actually to balance those
trees.

Let's do that today.

The Merkle tree/balanced binary tree that have data only in the leaf nodes
pushes new data down to the leaves, and if there is only one leaf in the parent
node, just adds it, appropriately, then rehashes everything that needs to be
rehashed, and if the parent node already has two leaves, creates a new parent,
shoves that into the tree and put the new node under that.

What happen if the new parent's parent already has two nodes.

See, that's why we have recursion, see?

So, a tree with one datum is easy:

Merkle (Twig (Leaf datum))

two leaves, we change the twig to a branch:

Merkle (Branch (Leaf a) (Leaf b))

Now, three leaves:

Merkle (Branch (Twig (Leaf a)) (Branch (Leaf b) (Leaf c))) or
Merkle (Branch (Branch (Leaf a) (Leaf b)) (Twig (Leaf c)))

as the balancing requires.

Today's haskell problem, get the ten latest summaries from blockchain.info
(you can use Data.BlockChain.Block.Summary functions above to help), and build
a Merkle tree/balanced binary tree using those summaries as leaves.

Note: as the summaries are already hashed, building a leaf becomes simpler.
--}

getSummaries :: [Hash] -> IO [Summary]
getSummaries = undefined

-- You can go to blockchain.info to get block summary hashs or you can use
-- 10hashes.txt at this directory instead, also available at this URL:
-- https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M09/D08/10hashs.txt

-- Now that you have the summaries, build a Merkle tree from them

blockSummaryTree :: [Summary] -> MerkleTree Summary
blockSummaryTree = undefined

-- You MAY wish to define an insert function for Merkle trees to help you to do it

insertLeaf :: Leaf a -> MerkleTree a -> MerkleTree a
insertLeaf = undefined

-- Okay! Have at it!
