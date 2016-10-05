module Y2016.M10.D05.Exercise where

-- Below imports available from cabal (snap-templates)

{--

If you do use the Snap framework, follow their guidance on building and 
deploying a web service

import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
--}

-- Below modules available from 1HaskellADay git repository

import Data.BlockChain.Block.Transactions (Transaction)
import Data.BlockChain.Block.Types (Hash)
import Data.Tree.Merkle (MerkleTree)

import Y2016.M09.D22.Exercise (latestTransactions)

{--
So, with the end-goal in sight, that is: to have the same Merkle tree everywhere,
we'd like to inject some realistic raison d'etre, some sine qua non, some
I-don't-know-what (Je ne sais quoi), into the problem space.

Because why? Because je ne sais pas.

(Ya see what I did with that? No? That's okay.)

So, if we simply wished to ensure the Merkle tree was the same everywhere, we'd
simply store it centrally and that would be everyone's single point of failure.

No. I meant to say, 'everyone's point of access,' it just came out right, is all.

You get that as you're sipping your Pumpkin Spiced Latte, I'm sure.

So, one thing of being a node is that we each have a Merkle tree, that gives
the block chain its decentralization. So we all have a Merkle tree. Great.

... yeah. great.

Well, now we need to compare Merkle trees. We can do a very fast compare by
comparing hashes. The same (root) hash means the same Merkle tree (with a very
high probability of correctness. Very high meaning _VERY_ high). And, before,
we have seen how to determine very quickly if root hashes are different which
(few) nodes of the tree are different.

Good. Good.

Now that we have a difference, we need to retrieve from one Merkle tree the
block (or whatever) that is different and then send just that block over the
wire to the requester, so that client can repair their Merkle tree and then
we can compare hashes again, hoping that we now have the same Merkle tree, and
if so, we're done: the environment is consistent again, and if not, we just
simply repeat, exchanging information until we achieve consistency.

So, before we can copy (bits of) the Merkle tree, we need to request a particular
node of the Merkle tree by hash and then, as the server, send that requested
node.

Let's do that today.

Today's Haskell problem.

Create a web service that maintains a Merkle tree. In this case, let's say
it's a mini-block chain of just the transactions of the latest block.

Design the web service so that a client may GET a node of that Merkle tree
by GET request of the hash ID of that node. Make it REST, because less 
headaches that way.

Stand up your web service. Make a few queries. Cheers!

--}

webService :: IO ()
webService = undefined

transaction :: MerkleTree Transaction -> Hash -> Maybe Transaction
transaction tree ident = undefined

-- you may wish to consider using a framework to do this, e.g.: Snap
-- http://snapframework.com

{-- BONUS -----------------------------------------------------------------

Have the web service return all the hash ids of all the nodes it contains.
Display it in some webby-REST-y whatever yummy way you want to show it.

--}

allHashes :: MerkleTree Transaction -> IO [Hash]
allHashes tree = undefined
