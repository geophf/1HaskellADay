module Y2016.M09.D28.Exercise where

-- below imports available from 1HaskellADay git repository

import Data.Monetary.BitCoin
import Data.Tree.Merkle
import Y2016.M09.D26.Exercise (coins2Merk, coins)

{--
Looking at a graphical representation of the Merkle tree with a new node
inserted we see several 'twigs,' that is to say nodes with only one leaf.

In merkle-tree-a (the png in this directory or at 

https://github.com/geophf/1HaskellADay/blob/master/exercises/HAD/Y2016/M09/D28/merkle-tree-a.png

We see four leaves but three nodes. It would make better balance to have just
two branches, not three.

The same can be said for merkle-tree-bb, that is merkle-tree-a but with an
additional insertion of a new leaf. You can see in in this directory or at:

https://github.com/geophf/1HaskellADay/blob/master/exercises/HAD/Y2016/M09/D28/merkle-tree-bb.png

Here again, we have three twigs, for a total of 4 branches, but two of the
twigs could have been combined into one branch, reducing the total number of
branches to 3, not 4.

The problem is in the way I defined insertLeaf and its helper function. I am
too eager to create a twig and not that eager to use an existing twig if
present.

Let's change that.

Define a new insertLeaf function, call it addLeaf so that, as before, it puts
the leaf in the approapriately hashed position, but this time a little smarter:

If it's being added to a twig, recreate that twig as a branch (as before.

BUT

If it's being added to a branch, FIRST check if there is an adjacent twig
to the left or to the right, and redistribute the leaves taking advantage
of that adjacent twig.
--}

addLeaf :: Leaf a -> MerkleTree a -> MerkleTree a
addLeaf leaf branch = undefined

{--
Now.

How do we know we've improved things? Visual inspection? Well, that's one way,
but a tedious way, to be sure. Let's instead, have a function that counts all
the branches of a MerkleTree.
--}

countBranches :: MerkleTree a -> Int
countBranches tree = undefined

{--
Now, with the data from before, create a Merkle Tree using insertLeaf and a
Merkle Tree using addLeaf. Does the addLeaf solution have the same or fewer
branches than the insertLeaf one?
--}

a, bb :: MerkleTree BitCoin
a = coins2Merk coins
bb = undefined          -- insert or add leaf of BTC 7.7 to a
