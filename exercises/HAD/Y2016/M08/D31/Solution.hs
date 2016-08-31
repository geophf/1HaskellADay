module Y2016.M08.D31.Solution where

import Control.Arrow ((>>>), (&&&))
import Crypto.Hash
import qualified Data.ByteString.Lazy.Char8 as BL

-- the below import available from 1HaskellADay git repository

import Control.Logic.Frege (adjoin)

{--
We'll look at Merkle trees from the tweet we saw yesterday on @1HaskellADay
from Carlos Galdino @carlosgaldino.

http://blog.carlosgaldino.com/merkle-trees.html

Also, Merkle trees are used in the Blockchain (which BitCoin uses), so here's
an article on that.

http://chimera.labs.oreilly.com/books/1234000001802/ch07.html#merkle_trees

We're not going to declare and construct Merkle trees today, what we are
going to do is to get warmed up with hashing functions, specifically the
SHA256 hashing function.

Define a function that takes a string and hashes it to an SHA256 digest:
--}

hashShow :: Show a => a -> Digest SHA256
hashShow = hashlazy . BL.pack . show

-- actually, hashShow works on anything that has a String-representation.

-- Hash the following strings:

hashme :: [String]
hashme = ["I like to move it, move it!", "π is 3.14159265358979323846..."]

-- what are the SHA256 hashes of the above strings?

{--
*Y2016.M08.D31.Solution> mapM_ (print . hashShow) hashme ~> hashes0
df8c3ee1b1a5b4201cb2baf8df3cbc2e008189eba983a6ca2e296100e15a1e62
0dd10591d26d65e6334a0d8eb08432757dcde511ef3fde01ab29daa1a8613302
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
*Y2016.M08.D31.Solution> mapM_ (print . hashhash) hashes0 ~> hashes1
dd414391543bf5a9fc368a512a788e5776a9fcd23066800ac5d3bd2aab5da3c6
8acff9a3032ef8657bea2f3893408dd55b54158502bbda8b86dc1f1b0b134b34

verified by eyeballs, the hashes are different.
--}

{--
So, here's the other thing. The Merkle tree's data nodes (leaf nodes) hash the 
data. Great, but nodes that contain (two) child nodes contain no data but
do contain a hash that is the hash of the concatenation of the hashes of its
(two) child nodes. Let's do that. Concatenate two hashes and then hashhash the
resulting String ... result.
--}

childrenHash :: Digest SHA256 -> Digest SHA256 -> Digest SHA256
childrenHash = curry (adjoin show >>> uncurry (++) >>> hashShow >>> hashhash)

-- Take the two hashed hashes from the above strings, concatenate them, then
-- hash hash that concatenation (of the two hash hashes). What is your result?

{--
*Y2016.M08.D31.Solution> uncurry childrenHash $ (head &&& last) hashes1 ~>
4b222f0abfc954557b985d8b0dce3800225285755b0102bed06e1993dd584226
--}
