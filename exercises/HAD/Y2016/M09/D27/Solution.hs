module Y2016.M09.D27.Solution where

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

import Control.Logic.Frege ((-|))
import Data.BlockChain.Block.Transactions
import Data.BlockChain.Block.Types
import Data.Tree.Merkle

import Y2016.M09.D22.Solution (latestTransactions)

-- hint: in a previous exercise you've defined latestTransactions
-- (see import above).

type Direction a = Either (Branch a) (Branch a)

pathTo :: Hash -> MerkleTree a -> ([Direction a], Maybe a)
pathTo h = pt [] h . root

pt :: [Direction a] -> Hash -> Branch a -> ([Direction a], Maybe a)
pt path h (Twig h' a) = if h == h' then (path, Just $ packet a) else noAns
pt path h b@(Branch _ l r) =
   if h == dataHash l then genAns Left l
   else if h == dataHash r then genAns Right r
   else noAns 
      where genAns dir leaf = (path ++ [dir b], Just $ packet leaf)
pt path h (Parent _ l r) =
   if h < leastHash l then noAns
   else if h < leastHash r then pt (path ++ [Left $ branch l]) h (branch l)
   else pt (path ++ [Right $ branch r]) h (branch r)

noAns :: ([Direction a], Maybe a)
noAns = ([], Nothing)

-- Since the Merkle tree is a binary tree, the path returned will be of the
-- form ([Left a, Left b, Left c, Right d], e) showing which direction through
-- the tree was taken to get to e.

{--
*Y2016.M09.D27.Solution> latestTransactions ~> x ~> length ~> 1811
*Y2016.M09.D27.Solution> let xleaf x = Leaf (hashCode x) x
*Y2016.M09.D27.Solution> let merk = fromList (map xleaf x)

-- FIRST TRANSACTION -------------------------------------------------------
*Y2016.M09.D27.Solution> let (p1, ans) = pathTo (hashCode (head x)) merk
*Y2016.M09.D27.Solution> length p1 ~> 17
*Y2016.M09.D27.Solution> ans
Just (TX {lockTime = 0, version = 1, size = 116
      inputs = [In {sequence = 2975175314, prevOut = Nothing, inScript = "..."}],
      time = 1475455895, txIndex = 179143255, vInSize = 1, vOutSize = 1, 
      hashCode = "e79c284d8fb16ac53af0dc3f5371a5213d71ae8641fd470dcc0618597375816e", ...})
*Y2016.M09.D27.Solution> p1
[Left Parent,Right Parent,Left Parent,Left Parent,Left Parent,Left Parent,
 Left Parent,Left Parent,Left Parent,Left Parent,Left Parent,Left Parent,
 Left Parent,Left Parent,Left Parent,Left Branch,Left Branch]

-- LAST TRANSACTION -------------------------------------------------------
*Y2016.M09.D27.Solution> let (pn, ans) = pathTo (hashCode $ last x) merk

*Y2016.M09.D27.Solution> ans
Just (TX {lockTime = 432579, version = 1, size = 226, inputs = ... ins ..{
*Y2016.M09.D27.Solution> length pn ~> 11
*Y2016.M09.D27.Solution> pn ~>
[Left Parent,Left Parent,Left Parent,Left Parent,Left Parent,Left Parent,
 Right Parent,Left Parent,Left Parent,Left Branch,Left Branch]

WOOT!
--}

