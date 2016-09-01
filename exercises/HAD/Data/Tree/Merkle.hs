module Data.Tree.Merkle where

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

childrenHash :: Digest SHA256 -> Digest SHA256 -> Digest SHA256
childrenHash = curry (adjoin show >>> uncurry (++) >>> hashDatum)

-- Take the two hashed hashes from the above strings, concatenate them, then
-- hash hash that concatenation (of the two hash hashes). What is your result?

{--
*Data.Tree.Merkle> uncurry childrenHash $ (head &&& last) hash0
90df6361bb3159f83c4b0554d5cf2331a6826b6cd0fe8ad83c61d80569e39a86
--}
