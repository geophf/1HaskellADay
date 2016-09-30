module Y2016.M09.D29.Solution where

import Control.Arrow ((&&&), second)
import Control.Monad ((>=>))
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord
import Data.Time.LocalTime

-- below imports available via 1HaskellADay git repository

import Data.BlockChain.Block.Transactions
import Data.BlockChain.Block.Types
import Data.Monetary.BitCoin
import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MM
import Data.Tree.Merkle

import Y2016.M09.D22.Solution (latestTransactions)
import Y2016.M09.D23.Solution (val2BTC)

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

-- it's helpful to make a Merkle Tree foldable

instance Foldable MerkleTree where
   foldr f z = foldr f z . root

instance Foldable Branch where
   foldr f z (Twig _ (Leaf _ v)) = f v z
   foldr f z (Branch _ (Leaf _ v1) (Leaf _ v2)) = f v1 (f v2 z)
   foldr f z (Parent _ bb1 bb2) = foldr f (foldr f z (branch bb1)) (branch bb2)

addresses :: MerkleTree Transaction -> AddrTransactions
addresses = MM.fromList pure
          . concatMap (uncurry zip . (mapMaybe (prevOut >=> addr) . inputs &&& repeat))
          . toList

{--
*Y2016.M09.D29.Solution> latestTransactions ~> lt ~> length ~> 2458
*Y2016.M09.D29.Solution> let tree = fromList (map mkleaf lt)
*Y2016.M09.D29.Solution> length tree ~> 2458 -- so foldable is WORKING!
*Y2016.M09.D29.Solution> let addrs = addresses tree
*Y2016.M09.D29.Solution> head $ MM.toList addrs ~>
("1128iYdsZB4CyV2F8T9bk1MzqRDjs6FeLZ",[TX {lockTime = 0, version = 1, ...}])

-- which address has the most transactions?

*Y2016.M09.D29.Solution> let sorted = sortBy (compare `on` Down . length . snd) (MM.toList addrs)
*Y2016.M09.D29.Solution> let ans = head sorted
*Y2016.M09.D29.Solution> fst ans ~> "1318wvPUgwkcDVzUyabuvduguhKM6piRfn"
*Y2016.M09.D29.Solution> length $ snd ans ~> 98

-- which address has the transaction involving the most bitcoins exchanged

*Y2016.M09.D29.Solution> let bits = map (second (maximum . map (val2BTC . sum . map value . out))) sorted
*Y2016.M09.D29.Solution> let sortedbits = sortBy (compare `on` Down . snd) bits
*Y2016.M09.D29.Solution> head sortedbits ~>
("13XTjbT68K7S3s1cEPBUs2uohEAjj3AYpv",BTC 416.24)
--}

-- Now: how do I find only the transactions between one address and another?

transactionsBetween :: AddrTransactions -> Hash -> Hash -> [Transaction]
transactionsBetween addrs me you =
   filter (elem you . mapMaybe addr . out) $ MM.lookup me addrs

{--
For a new map, largest address-set is 39
*Y2016.M09.D29.Solution> second length $ head sorted ~>
("1FMJXNWyNkWohjU6r6SMbwnt4b9nHXJUJE",39)

Huh, some transactions are just between only two parties anyway:

*Y2016.M09.D29.Solution> let x = transactionsBetween addrs "1FQ4SuwadRLnx6fNDvcs4Fy3KiLFkyQF9P" "1EMs2Acsr6groSFAume73LgE1wzJdz5d3P"
*Y2016.M09.D29.Solution> length x ~>

... which are all the transactions for that address.
--}

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

-- We'll solve this bonus problem ... AFTER I get some sleep!
