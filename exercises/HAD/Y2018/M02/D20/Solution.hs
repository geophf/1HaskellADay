module Y2018.M02.D20.Solution where

{--
From our favorite Prolog problem archive, P99:

http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/

P14 (*) Duplicate the elements of a list.
Example:
?- dupli([a,b,c,c,d],X).
X = [a,a,b,b,c,c,c,c,d,d]
--}

dupli :: [a] -> [a]
dupli list = list >>= replicate 2

{--
>>> dupli list0nums 
[50,50,88,88,47,47,46,46,55,55,35,35,4,4,66,66,27,27,65,65]

Some answers from the Twitterverse: 

Denis Stoyanov @xgrommx

I might be wrong but
`dupli = concatMap (\x -> [x,x])`
is pretty easy?

yes, also `dupli = (replicate 2 =<<)`

Bazzargh @bazzargh
 
(ap (:) (:[]) =<<), sum, and (2*).sum ? (fixing typo)

Johannes Weiß @johannesweiss
 
dupli = flip (>>=) (\x -> [x, x]) ?
--}

-- What is the sum of a list of numbers?
-- What is the sum of a list of duplicated numbers?

list0nums :: [Int]
list0nums = [50,88,47,46,55,35,4,66,27,65]

{--
>>> sum list0nums 
483
>>> sum (dupli list0nums)
966
--}

-- numbers generated at random.org

