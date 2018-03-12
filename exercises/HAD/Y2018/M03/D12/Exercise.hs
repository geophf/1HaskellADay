module Y2018.M03.D12.Exercise where

-- Let us return to Tarski's "Introduction to Logic." Where we left off at...

import Y2018.M03.D06.Exercise

{--
... we had defined the structure of curried functions, let us, today, reify
the function structure into that of expressions or designatory functions (easy
enough) but then put that into the whole of Tarski's logic thus far (not so 
easy) to express the sentence:

	Jenny's mom is Tom's mom
--}

import Y2018.M03.D05.Exercise (ATOM, RESOLUTOR)

data K = ConstantTypesIncludingTheFunType

data FUNCTION k = OurCurriedFunctionType

data EXPRESSION k = Expr (FUNCTION k) | Atom (ATOM k)

data SENTENCE k = Equation (EXPRESSION k) RESOLUTOR (EXPRESSION k)

{--
With the new SENTENCE and EXPRESSION types defined above redefine the below
arithmetic operators, redefine the solutions in the new Types declared above.
--}

add, pow, (%), minus :: K
(%)   = undefined
add   = undefined
pow   = undefined
minus = undefined

{--
Re-express these in the new logic:

(a) x is divisible by 3
(b) the sum of the numbers x and 2
(c) y^2 - z^2
(d) y^2 = z^2
(e) x + 2 < y + 3
(f) (x + 3) - (y + 5)
--}

a, b, c, d, e, f :: Either (EXPRESSION K) (SENTENCE K)
a = undefined
b = undefined
c = undefined
d = undefined
e = undefined
f = undefined

-- And now, represent the same-mother SENTENCE above:

momsOf :: SENTENCE Person
momsOf = undefined
