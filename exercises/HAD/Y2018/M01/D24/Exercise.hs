module Y2018.M01.D24.Exercise where

{--
Continuing on with our list exercises from P99, we look at P09 - P12. There may
be functions defined in Data.List that cover these, but I'll let you find that
out yourself.

P09 (**) Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists.

Example:
?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
--}

sample :: String
sample = "aaaabccaadeeee"

pack :: Eq a => [a] -> [[a]]
pack list = undefined

{--
>>> pack sample
["aaaa","b","cc","aa","d","eeee"]

P10 (*) Run-length encoding of a list.
Use the result of problem P09 to implement the so-called run-length encoding 
data compression method. Consecutive duplicates of elements are encoded as 
terms [N,E] where N is the number of duplicates of the element E.

Example:
?- encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
X = [[4,a],[1,b],[2,c],[2,a],[1,d][4,e]]
--}

encode :: Eq a => [a] -> [(Int, a)]
encode list = undefined

{--
>>> encode sample
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

P12 (**) Decode a run-length encoded list.
Given a run-length code list generated as specified in problem P10. Construct 
its uncompressed version.
--}

decode :: [(Int, a)] -> [a]
decode pairs = undefined

{--
>>> decode (encode sample)
"aaaabccaadeeee"

>>> decode (encode sample) == sample
True
--}

-- I'm also reading Tarski's Introduction to Logic, 1933, so some Haskell
-- problems may come from that.
