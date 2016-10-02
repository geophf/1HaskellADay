module Y2016.M09.D19.Solution where

import Control.Arrow ((&&&), (>>>))
import Data.Maybe (mapMaybe)

-- Below imports available in 1HaskellADay git repository

import Data.BlockChain.Block.Blocks
import Data.BlockChain.Block.Graphs
    hiding (REL, DATUM, TRANSACTION, BRANCH, START, ROOT, CHILD, Container, mar)
  -- the above hidden terms were introduced by working through this solution
  -- then were moved to the Data.BlockChain.Block.Graphs module
import Data.BlockChain.Block.Summary
import Data.BlockChain.Block.Transactions
import Data.BlockChain.Block.Types
import Data.Monetary.BitCoin
import Data.Relation
import Data.SymbolTable
import Data.Tree.Merkle

import Graph.JSON.Cypher
import Graph.Query

{--
Okay, from a couple of weeks ago, we are now creating Merkle trees that are
balanced, and we even fetched blocks from the block chain and created our own
Merkle tree.

GREAT!

The output, however, is rather on the heavy-side, from a massive wall of text
perspective. For the human eye, this leaves much to be desired.

Let's fix that.
 
Today, we are going to create a Merkle tree from some source data and then
visualize that Merkle tree in how-so you see fit.

1. We've already proved that we can fetch real blocks from the block chain
given a set of hashes, but let's simplify things a bit here. Let's reduce
a block to a simplified transaction. A transfers x BTC to B

The POS, point of sale or exchange for Bitcoin is not Person, but Address.

And we don't know what the addresses are (we do, but I don't want to dig into
those data now), but we can enumerate addresses with, e.g. SymbolTable.

-- type Name = String

-- data Person = P Hash Name deriving (Eq, Show)

peeps :: [Person]
peeps = undefined

-- define the function peeps such that it returns 10 people in howsoever you
-- wish to generate these people

data Xaction = X Person BitCoin Person deriving (Eq, Show)

xactions :: [Xaction]
xactions = undefined

-- Now, have xactions return 25 transcations, Person A transfers x BTC to
-- Person B. Again, define these transactions howso you wish
--}

-- So, what we're going to do to get some transactions is to ask for the latest
-- block and then extract the transactions from there, hoping there are a
-- sizeable number of transactions in that block (nowadays there usually are)

latestTransactions :: IO [Transaction]
latestTransactions =
   fmap (tx . head) (latestSummary >>= fetchBlocks . pure . blockHash)
   
{--
*Y2016.M09.D19.Solution> latestTransactions ~> xactions
*Y2016.M09.D19.Solution> length xactions ~> 895

So we have ~900 transactions, where TX { [Input] [Output BTC] } is roughly the
model. Let's model that (and by 'that' I mean 'Let's model that model.')

First, we extract and model the addresses from the set of transactions.

type Idx = Int
data Address = Addr { addrId :: Idx, addrHash :: Hash } deriving (Eq, Ord, Show)
--}

addresses :: Transaction -> [Hash]
addresses = mapMaybe addr . (mapMaybe prevOut . inputs &&& out >>> uncurry (++))

{--
*Y2016.M09.D19.Solution> head xactions 
TX {lockTime = 0, version = 1, size = 148, 
    inputs = [In {sequence = 4294967295, prevOut = Nothing, 
                  inScript = "035893060004ef55e357042c45433108d0c1a018c398555d0a425720537570706f727420384d200a666973686572206a696e78696e092f425720506f6f6c2f"}],
    time = 1474516463, txIndex = 176684638, vInSize = 1, vOutSize = 1, 
    hashCode = "5b026c39da86a21df06f8295b2a714b3ada33cad2a8fd9482babcd7c53492ded", 
    relayedBy = "123.56.129.45",
    out = [Out {spent = False, outTxIndex = 176684638, typ = 0, 
                value = 1267140415, n = 0, 
                addr = Just "1BQLNJtMDKmMZ4PyqVFfRuBNvoGhjigBKF", 
                outScript = "76a914721afdf638d570285d02d3076d8be6a03ee0794d88ac"}]}
*Y2016.M09.D19.Solution> addresses it ~> ["1BQLNJtMDKmMZ4PyqVFfRuBNvoGhjigBKF"]

Great, so we can put all these into a SymbolTable, so we have a manageable 
handle:

*Y2016.M09.D19.Solution Control.Monad.State> let st = execState (mapM fromEnumS (concatMap addresses xactions)) empty
*Y2016.M09.D19.Solution Control.Monad.State> top st ~> 2513

We have 2513 unique addresses on 

*Y2016.M09.D19.Solution Control.Monad.State> length xactions ~> 634

transactions. Ugh, but, oh, well! It's the nature of this beast.

beast, n.: decentralized network.

viewmerk :: [Transaction] -> IO ()
viewmerk = undefined

Solved in Y2016.M09.D23.Solution module
--}

-- Finally, put all those transactions into a Merkle tree, and then output
-- some visualization of that Merkle tree. Use whatever visualization tool
-- you prefer: SVG, Relations-as-Graph, or the Haskell diagram tool-suite.

-- Or come up with your own visualization.

-- We'll be looking at comparing, copying, and perhaps voting on Merkle trees
-- throughout the rest of this week.

mkTradeLeaf :: Transaction -> Leaf Trade
mkTradeLeaf = uncurry Leaf . (tradeHash &&& id) . tx2trade

data REL = START | CHILD | TRANSACTION deriving Show

instance Edge REL where
   asEdge = show

data Container a = ROOT (MerkleTree a) | BRANCH (Branch a) | DATUM (Leaf a)
   deriving Show

instance Node a => Node (Container a) where
   asNode (ROOT p) = "ROOT { " ++ addrRep (hashID (root p)) ++ " }"
   asNode (BRANCH b) = "BRANCH { " ++ addrRep (hashID b) ++ " }"
   asNode (DATUM f) = asNode (packet f)

merkAsRel :: MerkleTree a -> [Relation (Container a) REL (Container a)]
merkAsRel r = Rel (ROOT r) START (BRANCH (root r)):mar (root r)

mar :: Branch a -> [Relation (Container a) REL (Container a)]
mar p@(Parent h lb rb) =
   let [l,r] = map branch [lb, rb] in
   map (Rel (BRANCH p) CHILD . BRANCH) [l,r] ++ mar l ++ mar r
mar b@(Branch h l r) = 
   map (Rel (BRANCH b) TRANSACTION . DATUM) [l,r]
mar t@(Twig h d) = [Rel (BRANCH t) TRANSACTION (DATUM d)]

{--
*Y2016.M09.D19.Solution> latestTransactions ~> x
*Y2016.M09.D19.Solution> length x ~> 237
*Y2016.M09.D19.Solution> let trds = map mkTradeLeaf x
*Y2016.M09.D19.Solution> head trds
Leaf {dataHash = "dbac60c56454dcaeb65c55a4bde851372b56946725874af914475b12c3308ac7", 
      packet = Trd {tradeHash = "dbac60c56454dcaeb65c55a4bde851372b56946725874af914475b12c3308ac7", 
                    executed = 2016-09-26 01:59:19, ins = [Nothing], 
                    outs = [("1KFHE7w8BhaENAswwryaoccDb6qcT6DbYY",BTC 12.55)]}}
*Y2016.M09.D19.Solution> let merk = fromList trds
*Y2016.M09.D19.Solution> getGraphResponse ("http://neo4j:1234F0ul@127.0.0.1:7474/" ++ transaction)
                                  (map (mkCypher "a" "rel" "b") (merkAsRel merk))
"{\"results\":[{\"columns\":[],\"data\":[]},{\"columns\":[],\"data\":[]},...],\"errors\":[]}\n"

Shows the Merkle Tree with stubs of the transactions ... but, as this is a
graph database, we can load in the transactions and they will be liked in,
automagically!

*Y2016.M09.D19.Solution Graph.Query> getGraphResponse
                     ("http://neo4j:1234F0ul@127.0.0.1:7474/" ++ transaction) 
                     (map (mkCypher "a" "rel" "b") 
                          (concatMap (trade2relations . packet) trds))
... \"errors\":[]}\n"

Now the Merkle Tree includes the full transactions, as well
--}
