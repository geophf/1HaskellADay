module Y2018.M01.D22.Exercise where

{--
The P99 or the 99 Prolog problems are a wealth of 'how to do x' in logic
programming.

http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/

A lot of these problems Haskell solves already (e.g. in Data.List or Data.Map)
but some of the fun is defining the solution yourself or finding that library
function you didn't know existed.

Here's one: P21:

Insert an element at a given position into a list.
Example:
?- insert_at(alfa,[a,b,c,d],2,L).
L = [a,alfa,b,c,d]

or, put into Haskell-y terms:
--}

insertAt :: a -> [a] -> Int -> [a]
insertAt elt list position = undefined

{--
>>> insertAt "alfa" (words "a b c d") 2
["a","b","alfa","c","d"]

n.b. Prolog is 1-indexed, Haskell is 0-indexed. You see here I went with
Haskell's 0-index, but you can define the solution how-so-ever suits you.
--}
