module Y2016.M12.D14.Exercise where

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

type Qubit = Matrix (Complex Float)  --- where Float is 1 or 0 but okay

ket0, ket1 :: Qubit
ket0 = undefined
ket1 = undefined

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

type PauliOperator = Matrix (Complex Float)

pauliX, pauliY, pauliZ :: PauliOperator
pauliX = undefined
pauliY = undefined
pauliZ = undefined

-- exercise 3: rotate the qubits ket0 and ket1 through the pauliX operator
-- (figure out what that means). The intended result is:

-- X|0> = |1> and X|1> = |0>

-- what are your results?

rotate :: PauliOperator -> Qubit -> Qubit
rotate p q = undefined
