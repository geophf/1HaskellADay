module Data.Tree.Merkle where

{--
We'll look at Merkle trees from the tweet we saw yesterday on @1HaskellADay
from Carlos Galdino @carlosgaldino.

http://blog.carlosgaldino.com/merkle-trees.html

Also, Merkle trees are used in the Blockchain (which BitCoin uses), so here's
an article on that.

http://chimera.labs.oreilly.com/books/1234000001802/ch07.html#merkle_trees
--}

import Control.Arrow ((>>>), (&&&))
import Crypto.Hash
import qualified Data.ByteString.Lazy.Char8 as BL

-- the below import available from 1HaskellADay git repository

import Control.Logic.Frege ((<<-), adjoin)
import Data.Monetary.BitCoin

{-- HASHING -----------------------------------------------------------------

We're not going to declare and construct Merkle trees today, what we are
going to do is to get warmed up with hashing functions, specifically the
SHA256 hashing function.

Define a function that takes a string and hashes it to an SHA256 digest:
--}

hashShow :: Show a => a -> Digest SHA256
hashShow = hashlazy . BL.pack . show

-- actually, hashShow works on anything that has a String-representation.
-- And we want that hash to be doublely hashed so:

hashDatum :: Show a => a -> Digest SHA256
hashDatum = hashShow . show . hashShow

-- Hash the following strings:

hashme :: [String]
hashme = ["I like to move it, move it!", "π is 3.14159265358979323846..."]

-- what are the SHA256 hashes of the above strings?

{--
*Data.Tree.Merkle> mapM_ (print . hashDatum) hashme ~> hash0
18602aedaaecb9629303add65c23e7a53753d672ac0150cea9c838ca3c69512f
e08e9c734649decf1e099179d85b6b3e8fc7f0fdd7a2f8c4e09d1286e4a59838
--}

{--
So, here's the thing. Bitcoin doubly hashes the string, or, more correctly,
it hashes the exchange in the block, then it hashes that hash. Let's do the
same thing:

Fortunately, as Digest SHA256 is a show instance, we just call hashShow again:
--}

hashhash :: Digest SHA256 -> Digest SHA256
hashhash = hashShow

-- note that the hash of a hash is a hash. join function on monad, anyone?
-- So: hash the above strings, then hash the hashes. Verify that the hash
-- of a hash is not the original hash.

{--
*Y2016.M08.D31.Solution> mapM_ (print . hashhash) hash0 ~> hashes1
f80961602c4ea1b17ede78ede6ea7d9ac43229dc70f77129dedf3f6e0786664e
e236d4d55f3da96c7aa5ec8c4896622abe0ae13e2d2339edf71b04c4df040fdb

verified by eyeballs, the hashes are different.
--}

{--
So, here's the other thing. The Merkle tree's data nodes (leaf nodes) hash the 
data. Great, but nodes that contain (two) child nodes contain no data but
do contain a hash that is the hash of the concatenation of the hashes of its
(two) child nodes. Let's do that. Concatenate two hashes and then hashhash the
resulting String ... result.
--}

hash256 :: String -> Digest SHA256
hash256 = hashlazy . BL.pack

childrenHash :: Hash -> Hash -> Hash
childrenHash = show . hash256 <<- (++)

-- Take the two hashed hashes from the above strings, concatenate them, then
-- hash hash that concatenation (of the two hash hashes). What is your result?

{--
*Data.Tree.Merkle> uncurry childrenHash $ (head &&& last) hash0
90df6361bb3159f83c4b0554d5cf2331a6826b6cd0fe8ad83c61d80569e39a86
--}

{-- MERKLE TREES ------------------------------------------------------------

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

data MerkleTree a = Merkle { root :: Branch a }
   deriving (Eq, Ord, Show)

type Hash = String   -- because I can't convert String -> Digest SHA256
                     -- even from (hash str)

data Branch a = Parent { hashID :: Hash, leftBr, rightBr :: Branch a }
              | Branch { hashID :: Hash, leftLf, rightLf :: Leaf a }
              | Twig   { hashID :: Hash, soleLf :: Leaf a }
   deriving (Eq, Ord, Show)

data Leaf a = Leaf { dataHash :: Hash, packet :: a }
   deriving (Eq, Ord, Show)

-- Ya see what I did with branch, didja? A branch has either branches OR it
-- has two leaf nodes, a branch does NOT have a mix of the two!

-- Perhaps there is some generic way to express branch so that this distinction
-- is clear without muddling things with different type values of Parent and
-- Branch ... thoughts on this?

-- Now, we do a simple leaf and branch construction.

mkleaf :: Show a => a -> Leaf a
mkleaf = uncurry Leaf . (show . hashDatum &&& id)

things3 :: [BitCoin]
things3 = map BTC [4.4,1.2,9.6]

{--
*Y2016.M09.D01.Solution> let leaves = map mkleaf things3 
[Leaf {dataHash = b75a..., packet = BTC 4.40},
 Leaf {dataHash = 4bde..., packet = BTC 1.20},
 Leaf {dataHash = 2ef5..., packet = BTC 9.60}]
--}

-- makes a branch that has only 1 leaf, which we 'duplicate':

-- Actually, I find typing a sole-child branch differently (as a 'Twig') will
-- make replacement-on-insert easier down the road.

mkbranch1 :: Leaf a -> Branch a
mkbranch1 = uncurry Twig
          . (uncurry childrenHash . (dataHash &&& dataHash) &&& id)

{--
*Y2016.M09.D01.Solution> mkbranch1 (last leaves) ~> tw
Twig {hashID = 346b..., soleLf = Leaf {dataHash = 2ef5..., packet = BTC 9.60}}
--}

mkbranch :: Leaf a -> Leaf a -> Branch a
mkbranch a b = Branch ((uncurry childrenHash . adjoin dataHash) (a,b)) a b

{--
*Y2016.M09.D01.Solution> mkbranch (head leaves) (head $ tail leaves) ~> br ~>
Branch {hashID = e3b2..., 
        leftLf = Leaf {dataHash = b75a..., packet = BTC 4.40},
        rightLf = Leaf {dataHash = 4bde..., packet = BTC 1.20}}
--}

-- one more thing: let's construct a parent branch from two child branches

mkparent :: Branch a -> Branch a -> Branch a
mkparent a b = Parent (childrenHash (hashID a) (hashID b)) a b

{--
*Y2016.M09.D01.Solution> mkparent br tw ~>
Parent {hashID = 3c56..., 
        leftBr = Branch {hashID = e3b2..., 
                         leftLf = Leaf {dataHash = b75a..., packet = BTC 4.40},
                         rightLf = Leaf {dataHash = 4bde..., packet = BTC 1.20}},
        rightBr = Twig {hashID = 346b...,
                        soleLf = Leaf {dataHash = 2ef5..., packet = BTC 9.60}}}
--}
