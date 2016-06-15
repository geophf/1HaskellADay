module Control.Boolean where

-- http://lpaste.net/108295

import Prelude hiding (and, or, not)

infixr 2 -|

{--

A solution to the Boolean logic exercise at http://lpaste.net/108272

Okay, so I guess we are doing this thing.

From the P-99 Prolog problem set we are looking at P46 through P48: 
define truth tables for and, or, nand, nor, xor, impl, equ, and not. 

There's a twist to this problem, however:

You only have one, count'm: one, operator in the base language from which
you define all the other operators, so choose wisely.

We choose the nand-approach for this solution set. The nor-approach also
provides complete coverage of boolean logic.

 --}

and, or, nand, nor, xor, impl, equ :: Bool -> Bool -> Bool

nand True True = False
nand _    _    = True
-- tt: t t = f, t f = t, f t = t, f f = t

and a b = not (nand a b)
-- tt: t t = t, t f = f, f t = f, f f = f

or a b = nand (not a) (not b)
-- tt: t t = t, t f = t, f t = t, f f = f

nor a b = not (or a b)
-- tt: t t = f, t f = f, f t = f, f f = t

xor a b = not (equ a b)
-- tt: t t = f, t f = t, f t = t, f f = f

impl a b = nand a (not b)
-- tt: t t = t, t f = f, f t = t, f f = t

(-|) :: Bool -> Bool -> Bool
a -| b = impl a b

equ a b = or (and a b) (nor a b)
-- tt: t t = t, t f = f, f t = f, f f = t

not :: Bool -> Bool
not = nand True
-- tt: t = f, f = t

{--

AND THEN, once you've defined your operators, define a function, table,
that returns a table of truth values for an input expression: --}

table :: [(Bool, Bool)] -> (Bool -> Bool -> Bool) -> [Bool]
table truths fn = map (uncurry fn) truths

-- please note I changed the signature of table so that it would
-- unwrap the arguments directly with uncurry.

testBool :: (Bool -> Bool -> Bool) -> [Bool]
testBool = table [(a, b) | a <- [True, False], b <- [True, False]]

{--

So, you get this:

*Control.Boolean> testBool and
[True,False,False,False]
*Control.Boolean> testBool nor
[False,False,False,True]
*Control.Boolean> testBool xor
[True,False,False,True]
*Control.Boolean> testBool equ
[False,True,True,False]
*Control.Boolean> testBool impl
[True,False,True,True]

A little bit of history from wikipedia:

Boole's work and that of later logicians initially appeared to have no 
engineering uses. Claude Shannon attended a philosophy class at the 
University of Michigan which introduced him to Boole's studies. Shannon 
recognised that Boole's work could form the basis of mechanisms and 
processes in the real world and that it was therefore highly relevant. 
In 1937 Shannon went on to write a master's thesis, at the Massachusetts 
Institute of Technology, in which he showed how Boolean algebra could 
optimise the design of systems of electromechanical relays then used in 
telephone routing switches. He also proved that circuits with relays could 
solve Boolean algebra problems. Employing the properties of electrical 
switches to process logic is the basic concept that underlies all modern 
electronic digital computers. Victor Shestakov at Moscow State University 
(1907–1987) proposed a theory of electric switches based on Boolean logic 
even earlier than Claude Shannon in 1935 on the testimony of Soviet 
logicians and mathematicians Yanovskaya, Gaaze-Rapoport, Dobrushin, 
Lupanov, Medvedev and Uspensky, though they presented their academic 
theses in the same year, 1938.[clarification needed] But the first 
publication of Shestakov's result took place only in 1941 (in Russian). 
Hence Boolean algebra became the foundation of practical digital circuit 
design; and Boole, via Shannon and Shestakov, provided the theoretical 
grounding for the Digital Age.[34]

https://en.wikipedia.org/wiki/George_Boole

 --}

