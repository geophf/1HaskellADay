module Data.Qubit where

import Data.Array (elems, listArray)
import Data.Complex

-- import available via 1HaskellADay git repository

import Data.Matrix

{--
I'm thinking about quantum computation. IBM has released a 5-qubit computer
for public experimentation. So, let's experiment.

One way to go about that is to dive right in, so, yes, if you wish: dive
right in.

Another approach is to comprehend the maths behind quantum computation.

So, let's look at that.

I was going to bewail that Shor's prime factors algorithm needs 7 qubits to
work, but, NEWSFLASH! IBM has added Shor's algorithm to their API, so ...

CANCEL BEWAILMENT.

*ahem*

Moving on.

First, let's look at qubits. Qubits are 'bra-ket'ted numbers (ket numbers)
with the representation

|0> =  | 1 |   or    |1> = | 0 |
       | 0 |               | 1 |

OOH! MATRICES!

exercise 1. Represent ket0 and ket1 states as matrices in Haskell
--}

data Qubit = Qbit (Matrix (Complex Float))  --- where Float is 1 or 0 but okay

instance Show Qubit where
   show (Qbit mat) = '|':simplify mat ++ ">"

simplify :: Matrix (Complex Float) -> String
simplify = showC . determinant . cross (fromLists [[0, 1]])

showC :: (Eq a, Show a, Num a, RealFrac a) => Complex a -> String
showC (x :+ 0) =  -- show x -- but I don't like |1.0> and |0.0> for the kets
   showSimplestX x
showC (0 :+ x) = (if x == 1 then "" else show x) ++ "i"
showC c = show c

showSimplestX :: (Show a, Num a, RealFrac a) => a -> String
showSimplestX x =
   (if (100 * floor x) == floor (100 * x) then show . floor else show) x

ket0, ket1 :: Qubit
ket0 = Qbit (fromLists [[1],[0]])
ket1 = Qbit (fromLists [[0],[1]])

{--
It MAY be helpful to have a show-instance of a qubit that abbreviates the
complex number to something more presentable. Your choice.

A qubit state is most-times in a super-position of |0> or |1> and we represent
that as 

|ψ> = α|0> + β|1>

And we KNOW that |α|² + |β|² = 1

YAY! Okay. Whatever.

So, we have a qubit at |0>-state and we want to flip it to |1>-state, or vice
versa. How do we do that?

We put it through a Pauli X gate

The Pauli X operator is =  | 0 1 |
                           | 1 0 |

That is to say, zero goes to 1 and 1 goes to zero.

excercise 2: represent the Pauli X, Y, and Z operators
--}

data PauliOperator = POp (Matrix (Complex Float))

data Cnum = C (Complex Float)

instance Show Cnum where show (C c) = showC c

printPauli :: PauliOperator -> IO ()
printPauli (POp mat) =
   pprint (M (listArray ((1,1),(2,2)) (map C (elems (matrix mat)))))

pauliX, pauliY, pauliZ :: PauliOperator
pauliX = POp (fromLists [[0,1],[1,0]])
pauliY = POp (fromLists [[0,0 :+ (-1)],[0 :+ 1,0]])
pauliZ = POp (fromLists [[1,0],[0,-1]])

{--
*Y2016.M12.D14.Solution> printPauli pauliX
Matrix 2x2
| 0.0 1.0 |
| 1.0 0.0 |
--}

-- exercise 3: rotate the qubits ket0 and ket1 through the pauliX operator
-- (figure out what that means). The intended result is:

-- X|0> = |1> and X|1> = |0>

-- what are your results?

rotate :: PauliOperator -> Qubit -> Qubit
rotate (POp p) (Qbit q) = Qbit (cross p q)

{--
*Y2016.M12.D14.Solution> rotate pauliX ket0 ~> |1.0>
*Y2016.M12.D14.Solution> rotate pauliX ket1 ~> |0.0> 

BAM!
--}
