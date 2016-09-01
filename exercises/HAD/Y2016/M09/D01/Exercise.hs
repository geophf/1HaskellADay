module Y2016.M09.D01.Exercise where

import Crypto.Hash

-- below imports available from 1HaskellADay git repository

import Data.Monetary.BitCoin
import Data.Tree.Merkle

{--
So, yesterday we looked at hashing data then hashing the hashes to those data.

Why?

Well, because I said so! It has NOTHING to do with the fact that bitcoin does
this to store data on the blockchain. Nothing at all.

Today, we will look at rudimentary representation of Merkle trees.

Merkle trees are balanced binary trees that carry metadata, these metadata are
the hash of the data (if the node is leaf) or the hash of the child nodes (if
the node is not leaf). That means something: all non-leaf nodes, EXCEPT the
root, have two child nodes. What happens if there is a dangling leaf node?

The Merkle tree solves this by duplicating that leaf node.

So, if we have three leaf nodes: a, b, c, then the Merkle tree looks like this:

                 ROOT
                /    \
               x      y
              / \    / \
             a   b  c   c 

Got it? Great! If you don't, read up on Merkle trees. You do know how to wiki,
right?

So, today, we are going to be creating two constructs: Leaf and Branch, and
with that (eventually) we should be able to construct any Merkle tree.

Let's do this.
--}

type MerkleTree a = Branch a  -- that was hard

data Branch a = Parent { hashID :: Digest SHA256, leftBr, rightBr :: Branch a }
              | Branch { hashID :: Digest SHA256, leftLf, rightLf :: Leaf a }
   deriving (Eq, Ord, Show)

data Leaf a = Leaf { dataHash :: Digest SHA256, packet :: a }
   deriving (Eq, Ord, Show)

-- Ya see what I did with branch, didja? A branch has either branches OR it
-- has two leaf nodes, a branch does NOT have a mix of the two!

-- Perhaps there is some generic way to express branch so that this distinction
-- is clear without muddling things with different type values of Parent and
-- Branch ... thoughts on this?

-- Now, we do a simple leaf and branch construction.

mkleaf :: a -> Leaf a
mkleaf = undefined

mkbranch1 :: Leaf a -> Branch a
mkbranch1 = undefined         -- hint: remember to duplicate the sole leaf here

mkbranch :: Leaf a -> Leaf a -> Branch a
mkbranch = undefined          -- hint: see Data.Tree.Merkle.childrenHash

-- one more thing: let's construct a parent branch from two child branches

mkparent :: Branch a -> Branch a -> Branch a
mkparent = undefined          -- hint: ... same hint as mkbranch

-- With the above definitions, make a simple Merkle Tree with the below data

things3 :: [BitCoin]
things3 = map BTC [4.4, 1.2, 9.6]

-- yes, we are, strangely enough, making a Merkle tree of bitcoins. NEAT!

constructMerkleTree :: [a] -> MerkleTree a
constructMerkleTree = undefined      -- Hint: hmm, is a MerkleTree Foldable? hm

-- In future exercises we'll look at balance-on-inserting-new-nodes and copying

