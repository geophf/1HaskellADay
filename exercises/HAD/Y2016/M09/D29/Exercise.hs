module Y2016.M09.D29.Exercise where

import Data.Time.LocalTime

-- below imports available via 1HaskellADay git repository

import Data.BlockChain.Block.Transactions
import Data.BlockChain.Block.Types
import Data.MultiMap (MultiMap)
import Data.Tree.Merkle

{--
So, Merkle trees are effient for searching by hash, which makes copying them
a much smaller task than copying a whole Merkle tree. This is a good thing,
as the block chain, a Merkle tree, is 65+ gigabytes and growing.

We won't look at copying today.

What Merkle trees are not designed for is searching for tags or keys in the
leaves of the tree. This is a problem for someone who wishes to view their
latest transaction or some or all of their transactions but doesn't have the
hash of the transaction.

One could counter: just hash your id, the receiver's id, and the bitcoins
exchanged, right?

No, it's not that simple, as transactions often have third-party fees for 
processing the transaction, or have multiple inputs and outputs for one 
transaction. If you only hash yourself, the receiver and the amount, your hash
will likely not match the transaction you are looking for.

So, we need a way to find data in a Merkle tree without the hash.

This is a problem, because if we don't have the hash, we may have to search
the entire tree, every time, unguided.

That's a problem.

Today's Haskell exercise.

Given a Merkle tree, create auxiliary search structures that allow you to
extract transactions by address or by date/time range (and address).
--}

-- First populate a Merkle tree with transactions from the, e.g., latest block.

type AddrTransactions = MultiMap Hash Transaction [Transaction]

addresses :: MerkleTree Transaction -> AddrTransactions
addresses tree = undefined

-- which address has the most transactions?
-- which address has the transaction involving the most bitcoins exchanged

-- Now: how do I find only the transactions between one address and another?

transactionsBetween :: AddrTransactions -> Hash -> Hash -> [Transaction]
transactionsBetween addrs me you = undefined

{-- BONUS -----------------------------------------------------------------

Now we wish to time-box transactions. This will involve some faux data 
generation, unless, of course, you wish to scan the entire block chain into
your system and play with that.

Generate or read in a set of transactions for a set of users, making sure
these transactions fall over a set of different time-periods (e.g. days)

Using the above structure, have only the transactions returned within a time-
period

--}

transactionsFor :: AddrTransactions -> Hash -> LocalTime -> LocalTime
                -> [Transaction]
transactionsFor addresses user from to = undefined
