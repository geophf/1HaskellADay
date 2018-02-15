module Y2018.M02.D15.Solution where

{--
Another problem from P99: the late, great Prolog 99 problems set, reposed at:

http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/

(**) Determine the greatest common divisor of two positive integer numbers.
Use Euclid's algorithm.
Example:
?- gcd(36, 63, G).
G = 9
Define gcd as an arithmetic function; so you can use it like this:
?- G is gcd(36,63).
G = 9
--}

import Prelude hiding (gcd)

import Data.Set (Set)
import qualified Data.Set as Set

gcd :: Int -> Int -> Int
gcd x y = Set.findMax (Set.intersection (divisors x) (divisors y))

-- hint: there is always a GCD.

divisors :: Int -> Set Int
divisors x = Set.fromList (filter (\y -> mod x y == 0) (enumFromTo 1 x))

{--
>>> gcd 36 63
9
>>> gcd 35 63
7
>>> gcd 35 64
1

BUT OF COURSE gcd ALREADY EXISTS IN THE PRELUDE! HASKELL WINS AGAIN! AHA!
--}
