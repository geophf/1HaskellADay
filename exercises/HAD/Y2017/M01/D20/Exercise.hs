module Y2017.M01.D20.Exercise where

import Data.Traversable

-- below imports available via 1HaskellADay git repository

import Data.BlockChain.Block.Transactions
import Data.BlockChain.Block.Utils
import Data.Monetary.BitCoin
import Data.Tree.Merkle

import Y2017.M01.D16.Exercise
import Y2017.M01.D17.Exercise
import Y2017.M01.D18.Exercise
import Y2017.M01.D19.Exercise

-- Today we are going to make Merkle Trees Traversable instances

instance Functor Branch where
   fmap = undefined

instance Traversable Branch where
   traverse = undefined

-- that was easy.

{-- 
Now.

Load in the latest Block as a set of transactions, convert those transactions
into a Merkle tree and answer the below questions using your new-found
traversable-ness.
--}

-- 1. Find, if there is one, the First transaction that is more than 100 BTC

findTransactionGT :: MerkleTree Transaction -> BitCoin -> Maybe Transaction
findTransactionGT = undefined

-- hint: Merkle a is a wrapper around Branch a

-- 2. What is the date of lastest transaction in this block?

lastestTransaction :: MerkleTree Transaction -> Maybe Transaction
lastestTransaction = undefined
