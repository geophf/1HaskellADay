module Y2018.M02.D15.Exercise where

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

gcd :: Int -> Int -> Int
gcd x y = undefined

-- hint: there is always a GCD.
