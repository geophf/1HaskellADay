module Y2016.M08.D31.Exercise where

import Crypto.Hash
import qualified Data.ByteString.Lazy.Char8 as BL

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
hashShow = undefined

-- actually, hashShow works on anything that has a String-representation.

-- Hash the following strings:

hashme :: [String]
hashme = ["I like to move it, move it!", "π is 3.14159265358979323846..."]

-- what are the SHA256 hashes of the above strings?

{--
So, here's the thing. Bitcoin doubly hashes the string, or, more correctly,
it hashes the exchange in the block, then it hashes that hash. Let's do the
same thing:
--}

hashhash :: Digest SHA256 -> Digest SHA256
hashhash = undefined

-- note that the hash of a hash is a hash. join function on monad, anyone?
-- So: hash the above strings, then hash the hashes. Verify that the hash
-- of a hash is not the original hash.

{--
So, here's the other thing. The Merkle tree's data nodes (leaf nodes) hash the 
data. Great, but nodes that contain (two) child nodes contain no data but
do contain a hash that is the has of the concatenation of the hashes of its
(two) child nodes. Let's do that. Concatenate two hashes and then hashhash the
resulting String ... result.
--}

childrenHash :: Digest SHA256 -> Digest SHA256 -> Digest SHA256
childrenHash = undefined

-- Take the two hashed hashes from the above strings, concatenate them, then
-- hash hash that concatenation (of the two hash hashes). What is your result?

