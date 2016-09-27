module Y2016.M09.D27.Exercise where

{--
We're going to do a bit of Merkle Tree exploration.

So you have a Merkle tree; great! And you can compare it! Great!

1. Well, one thing to do is to copy it, so you have two (duplicate) Merkle trees.

2. Another thing to do is to update a branch of one Merkle tree with the branch
of another. That sounds simple, but this is deceptive. For the whole Merkle
tree, itself, is a branch, and a change to a node affects the hash at the main
branch. How do we know which node changed (or was added to, or whatever).

3. Another-ANOTHER thing to do is to improve my insert algorithm. I noticed 
yesterday that insert was too eager to insert new twigs (nodes with only one
leaf), even in the presence of collocated twigs. We must change that.

But not today.

Today, we're going to narrow down to a path through the Merkle tree so we may
address 2. above directly, and then help, eventually, with 1. above.

Let's do that.

Construct a Merkle tree from the transactions from the latest block (so each
leaf is a transaction), then, once constructed, find the leaf, using the hash,
of the first transaction and the last transaction of the block. Return the path
from the root all the way down to the leaf (exclusive), and then that leaf.
--}

-- below imports available from 1HaskellADay git repository

import Data.BlockChain.Block.Transactions
import Data.BlockChain.Block.Types
import Data.Tree.Merkle
import Y2016.M09.D22.Exercise (latestTransactions)

-- hint: in a previous exercise you've defined latestTransactions
-- (see import above).

pathTo :: MerkleTree a -> Hash -> ([Either (Branch a) (Branch a)], a)
pathTo tree hash = undefined

-- Since the Merkle tree is a binary tree, the path returned will be of the
-- form ([Left a, Left b, Left c, Right d], e) showing which direction through
-- the tree was taken to get to e.
