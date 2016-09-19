module Y2016.M09.D08.Solution where

import Control.Arrow ((&&&))
import Crypto.Hash
import Data.Function (on)
import Data.List (sortBy)

import qualified Data.ByteString.Char8 as B

-- Below available from 1HaskellADay git repository

import Control.Logic.Frege (adjoin)
import Data.BlockChain.Block.Blocks
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

getBlocks :: [Hash] -> IO [Block]
getBlocks = mapM (readBlock rawBlockURL)

readHashes :: FilePath -> IO [Hash]
readHashes = fmap lines . readFile

-- You can go to blockchain.info to get block summary hashs or you can use
-- 10hashes.txt at this directory instead, also available at this URL:
-- https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2016/M09/D08/10hashs.txt

-- *Y2016.M09.D08.Solution> readHashes "Y2016/M09/D08/10hashs.txt" ~> hashes
-- *Y2016.M09.D08.Solution> getBlocks hashes ~> blks

-- we use the block's hash as the leaf's hash
mkBlockLeaf :: Block -> Leaf Block
mkBlockLeaf = uncurry Leaf . (blockhash &&& id)

-- *Y2016.M09.D08.Solution> let levs = map mkBlockLeaf blks

insertLeaf :: Leaf a -> MerkleTree a -> MerkleTree a
insertLeaf l (Merkle x) = Merkle (insertL' l x)

{-- moved to Data.Tree.Merkle:

leavesHashes :: (Leaf a, Leaf a) -> Hash
leavesHashes = uncurry childrenHash . adjoin dataHash

kidsHashes :: (Branch a, Branch a) -> Hash
kidsHashes = uncurry childrenHash . adjoin hashID
--}

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

blockTree :: [Block] -> MerkleTree Block
blockTree = uncurry (foldr insertLeaf)
          . (Merkle . mkbranch1 . mkBlockLeaf . head &&& map mkBlockLeaf . tail)

{--
*Y2016.M09.D08.Solution> let blktr = blockTree blks
*Y2016.M09.D08.Solution> take 1000 (show blktr )
Merkle {root = Parent {hashID = "409304e6eca233505418d91c1cd3272be110dfbe5c5966fed205ae70a9549692",
                  leftBr = BB {leastHash = "0000000000000000009db44cfb6c1f652d8ee2014d957741582c5b955069ffc3",
                       branch = Parent {hashID = "bf8dc3622a7e33dd98f4d55e8bce887bcdd20b52e028a1a7fb276eaa685e0483",
                       leftBr = BB {leastHash = "0000000000000000009db44cfb6c1f652d8ee2014d957741582c5b955069ffc3",
                             branch = Parent {hashID = "c3f4f67c0aff18839335c463c6b48ac6dbe1134ef5c57ffbf26666d36899b34d", 
                                 leftBr = BB {leastHash = "0000000000000000009db44cfb6c1f652d8ee2014d957741582c5b955069ffc3",
                                        branch = Parent {hashID = "7596573c20cece81fe853de340cb7092af1d76604118cbdf7c831690aa5014c3",
                                                 leftBr = BB {leastHash = "0000000000000000009db44cfb6c1f652d8ee2014d957741582c5b955069ffc3",
                                                        branch = Parent {hashID = "3d13a87e2f44bdc31c66f598f86a933305226478c23eb8b26e3391db449ed4b7", 
                                                             leftBr = BB {leastHash = "0000000000000000009db44cfb6c1f652d8ee2014d957741582c5b955069ffc3",
                                                                   branch = Branch {hashID = "5b59f372f74e1ebf4b3ed4254105ef1e ...

YES, the visualization leaves much to be desired, but there you go, some blocks
in the blockchain 
--}

-- adding the insert function to the Data.Tree.Merkle module
