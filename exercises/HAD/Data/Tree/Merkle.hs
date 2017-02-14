module Data.Tree.Merkle where

{--
We'll look at Merkle trees from the tweet we saw yesterday on @1HaskellADay
from Carlos Galdino @carlosgaldino.
http://blog.carlosgaldino.com/merkle-trees.html
Also, Merkle trees are used in the Blockchain (which BitCoin uses), so here's
an article on that.
http://chimera.labs.oreilly.com/books/1234000001802/ch07.html#merkle_trees
--}

import Control.Arrow ((>>>), (&&&), app)
import Crypto.Hash
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function (on)
import Data.List (sortBy)

-- the below import available from 1HaskellADay git repository

import Control.Logic.Frege ((<<-), adjoin)
import Data.BlockChain.Block.Types (Hash)
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

{-- moved to Data.BlockChain.Block.Types
type Hash = String   -- because I can't convert String -> Digest SHA256
                     -- even from (hash str)
--}

data Branch a = Parent { hashID :: Hash, leftBr, rightBr :: BalancedBranch a }
              | Branch { hashID :: Hash, leftLf, rightLf :: Leaf a }
                       -- least hash is (dataHash . leftLf)
              | Twig   { hashID :: Hash, soleLf :: Leaf a }
                       -- least hash is (dataHash . soleLf)
   deriving (Eq, Ord)

instance Show (Branch a) where
   show (Parent _ _ _) = "Parent"
   show (Branch _ _ _) = "Branch"
   show (Twig _ _) = "Twig"

-- the fn least presupposes it is working with an already-balanced Merkle tree

least :: Branch a -> Hash
least (Twig _ sl) = dataHash sl
least (Branch _ ll _) = dataHash ll
least (Parent _ lb _) = leastHash lb

data BalancedBranch a = BB { leastHash :: Hash, branch :: Branch a }
   deriving (Eq, Ord)

-- n.b.: show instance for BalancedBranch is unnecessary, as this is a helper
-- type to Branch a

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

leavesHashes :: (Leaf a, Leaf a) -> Hash
leavesHashes = uncurry childrenHash . adjoin dataHash

kidsHashes :: (Branch a, Branch a) -> Hash
kidsHashes = uncurry childrenHash . adjoin hashID

-- makes a branch that has only 1 leaf, which we 'duplicate':

-- Actually, I find typing a sole-child branch differently (as a 'Twig') will
-- make replacement-on-insert easier down the road.

mkbranch1 :: Leaf a -> Branch a
mkbranch1 = uncurry Twig
          . (uncurry childrenHash . (dataHash &&& dataHash) &&& id)

{--
*Y2016.M09.D01.Solution> let tw = mkbranch1 (last leaves)
Twig {hashID = 346b..., soleLf = Leaf {dataHash = 2ef5..., packet = BTC 9.60}}
--}

mkbranch :: Leaf a -> Leaf a -> Branch a
mkbranch a b = Branch (leavesHashes (a,b)) a b

{--
*Y2016.M09.D01.Solution> let br = mkbranch (head leaves) (head $ tail leaves) ~>
Branch {hashID = e3b2..., 
        leftLf = Leaf {dataHash = b75a..., packet = BTC 4.40},
        rightLf = Leaf {dataHash = 4bde..., packet = BTC 1.20}}
--}

-- one more thing: let's construct a parent branch from two child branches

mkparentFromParents :: (BalancedBranch a, BalancedBranch a) -> Branch a
mkparentFromParents =
   app . (uncurry . Parent . (ua childrenHash (hashID . branch)) &&& id)

mkparent :: Branch a -> Branch a -> Branch a
mkparent = curry (mkparentFromParents . adjoin mkbb)

mkbb :: Branch a -> BalancedBranch a
mkbb = uncurry BB . (least &&& id)

ua :: (b -> b -> c) -> (a -> b) -> (a, a) -> c
ua g f = uncurry g . adjoin f

{--
*Y2016.M09.D01.Solution> mkparent br tw ~>
Parent {hashID = "6e84df977ad3e62ebb6348dba8b00ed2449f02a474711bd38a26b158c0e51d8e",
    leftBr = BB {leastHash = "b75afa7bfae31070012a57c5e76ae4b93b9585478e33c2ba897f25e33940e3cf", 
                 branch = Branch {hashID = "787a63f32768903d2c4fbd1e7e58d79a0298309be1d2e35f52076f31f6849ab9", 
                                  leftLf = Leaf {dataHash = "b75afa7bfae31070012a57c5e76ae4b93b9585478e33c2ba897f25e33940e3cf", 
                                                 packet = BTC 4.40},
                                  rightLf = Leaf {dataHash = "4bdede6162533ffcb79db0ceb6be2d318b86c34b1c69e2fbc7da109a69a83d0b", 
                                                  packet = BTC 1.20}}},
    rightBr = BB {leastHash = "2ef5d9f1f8a5412e23384041311da037ee4c55dd4575533157bbd5498bb77a69", 
                  branch = Twig {hashID = "54e4d0abf6b4a4efef62f96ca898fdc0b1fd7e42d84eef343131c4ad8ef98d24", 
                                 soleLf = Leaf {dataHash = "2ef5d9f1f8a5412e23384041311da037ee4c55dd4575533157bbd5498bb77a69", 
                                                packet = BTC 9.60}}}}
--}

-- INSERTION -----------------------------------------------------------------

{--
Okay, here we do balanced insertion into Merkle trees, with which we include
the fromList function.
--}

insertLeaf :: Leaf a -> MerkleTree a -> MerkleTree a
insertLeaf l (Merkle x) = Merkle (insertL' l x)

-- simple enough, eh?

insertL' :: Leaf a -> Branch a -> Branch a

-- simple case: we branch a twig:

insertL' l (Twig h l') = Branch (leavesHashes (l, l')) l l'

-- simple case: we parent a branch:

insertL' l (Branch h l' l'') =
   let [l1, l2, l3] = sortBy (compare `on` dataHash) [l, l', l'']
       br = Branch (leavesHashes (l1, l2)) l1 l2
       tw = Twig (dataHash l3) l3
   in  mkparent br tw

-- complex case: for a parent we have to cdr-down to the branch or twig
-- then recompute the hashes all the way back up to the root

insertL' l p@(Parent h b1 b2) =

-- but to cdr-down, we need to know the less-than/greater-than values of
-- the left/right branches. Hm. Why do I have to invent this?

-- Invented: put into Data.Tree.Merkle as part of the MerkleTree structure.

{--
There are 5 cases:
1. less than least lb b1, insert into b1
2. less than least rb b1, insert into b1
3. less than least lb b2, insert into b1
4. less than least rb b2, insert into b2
5. greater than least rb b2, insert into b2
After than decision matrix, we need to rehash whatever branch, all the way down.
... which means reconstructing this branch!
--}
   either (flip (curry mkparentFromParents) b2) (curry mkparentFromParents b1)
   $ insertLrehash l (if dataHash l < leastHash b2 then Left b1 else Right b2)

-- this gives the re-realized child branch, now we regraft it onto the parent
-- rehashing ... I think there's an ArrowChoice for all this?

type Eitherness a = Either (BalancedBranch a) (BalancedBranch a)

insertLrehash :: Leaf a -> Eitherness a -> Eitherness a
insertLrehash l (Left br) = Left . mkbb . insertL' l $ branch br
insertLrehash l (Right br) = Right . mkbb . insertL' l $ branch br

-- Now that you have the summaries, build a Merkle tree from them

fromList :: [Leaf a] -> MerkleTree a
fromList = uncurry (foldr insertLeaf) . (Merkle . mkbranch1 . head &&& tail)

-- n.b. must be non-empty list, because there's no empty MerkleTree.

-- FOLDABLE -----------------------------------------------------------------

instance Foldable MerkleTree where
   foldr f z = foldr f z . root

instance Foldable Branch where
   foldr f z (Twig _ (Leaf _ v)) = f v z
   foldr f z (Branch _ (Leaf _ v1) (Leaf _ v2)) = f v1 (f v2 z)
   foldr f z (Parent _ bb1 bb2) = foldr f (foldr f z (branch bb1)) (branch bb2)
