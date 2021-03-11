module Y2021.M03.D11.Solution where

import Data.Map (Map)
import qualified Data.Map as Map

{--
Today's #haskell problem, we're going to provide unique identifier for words.

This is very computer-science-y, and Haskell is well-suited to take on this
problem.

For example, variables, bound or unbound ('free'), are named 'foo,' 'bar,'
and 'quux' in your program ...

RIGHT? DON'T YOU NAME ALL YOUR VARIABLES THOSE NAMES, AND NOT 'i' and 'x'
BECAUSE THIS ISN'T FORTRAN, FOLKS, I HATE TO BREAK IT TO YOU!

*whew* I'm glad I kept my cool there. O.o

So, given a list of words, provide a set of unique identifiers for them.
--}

uniqueIds :: Ord a => [a] -> Map a Integer
uniqueIds = Map.fromList . flip zip [1..]

-- give a set of unique ids for the following

names :: [String]
names = words "Tom Dick Harry"

{--
>>> uniqueIds names
fromList [("Dick",2),("Harry",3),("Tom",1)]
--}
