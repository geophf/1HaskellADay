module Y2016.M10.D03.Exercise where

-- below imports available from 1HaskellADay git repository:

import Data.BlockChain.Block.Types
import Data.Tree.Merkle

import Y2016.M09.D22.Exercise (latestTransactions)

{-

*SIIIIIGGGGGGHHHHHH* <<- imagine the long sigh of a clerical worker when you
                         hand in your forms only in duplicate (not triplicate)
                         and filled out incorrectly, obviously (to the cleric)

So, PRINTING out, or the Show instance of Merkle trees was very easily declared
but a terrible mess to read, particularly when you're dealing with a block
that has 2332 transactions that go into the Merkle tree.

When I'm inspecting an instance of a tree-(like-)structure, I am less concerned
with all the details of each object of the tree, and am much more concerned
with the structure of the tree, itself.

One way to go about this is to restructure the tree so that the nodes (which
are branches) contain no structural information, so you have separate concerns:
the tree(-structure) and the nodes (the values) it contains.

That sounds too nice-n-clean for me right now: lots of work, lots of payoff.

Wait. Did I just say that?

Another way to go about this is to revise the Show instance of the tree-structure
so that it shows the structure only, and if you wish to inspect a particular
value, well, then, by Gum! You can do that on your own time, Charlie!

Let us choose the latter course.

Today's #haskell exercise: I've removed the Show derivation from the Branch a
and BalancedBranch a data types in Data.Tree.Merkle. Create a show instance
of these (containment-)structures that do not show all the messy details of
the values contained.
--}

instance Show (Branch a) where
   show = undefined   -- note: the a-type is not necessarily showable anymore

instance Show (BalancedBranch a) where
   show = undefined

-- I'm not too keen on seeing the SHA256 hashed values, either, by-the-bye.

-- With the above Show instances defined, load in latestTransactions and
-- show the Path to the first and the last transactions in that block

lengthToHash :: MerkleTree a -> Hash -> Int
lengthToHash tree hash = undefined

-- What is the length of the path to the first transaction?
-- What is the length of the path to the last transaction?
-- What is the mean length to any transaction in the latest block?
-- How many transactions were in the latest block?
 
-- THERE! Now isn't that better than looking at the entire tree at the head
-- of each path?
