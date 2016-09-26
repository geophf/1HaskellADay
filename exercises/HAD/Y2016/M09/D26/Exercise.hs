module Y2016.M09.D26.Exercise where

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

import Data.BlockChain.Block.Graphs
import Data.Monetary.BitCoin
import Data.Tree.Merkle

-- Create a Merkle Tree using the below BitCoin values as leaves -----------

coins :: [BitCoin]
coins = map BTC [5.7, 0.01, 1.02, 9.3]

coins2Merk :: [BitCoin] -> MerkleTree BitCoin
coins2Merk = undefined

-- Actually: create two trees, a and b, from the same set of coins.
-- Are a and b equivalent?

-- Now, insert BTC 7.7 to tree b.

-- return an ordered list of nodes of the Merkle tree that are different
-- between tree a and tree b

diffNodes :: MerkleTree a -> MerkleTree a -> [Branch a]
diffNodes a b = undefined

-- Here's the thing: you don't need to compare node-data. The nodes of a Merkle
-- tree are hashed, so you only need compare hsahID to see a difference.

-- hint: how are you going to iterate through the nodes of the trees?
