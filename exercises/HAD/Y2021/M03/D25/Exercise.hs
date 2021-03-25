module Y2021.M03.D25.Exercise where

{--
Oh, no! I've got bitten by the FORTH-bug. MAKE IT STOP!

(too late! dank, moody soundtrack)

So, yesterday, we created an RPN ('Reverse Polish Notation') calculator, but
I got to the point in my Gaussian sum where I wanted to do this:

2 SWAP / . 

... but I didn't have SWAP.

THE.
SADz!

So, let's add some 'minimal FORTH' operations to our RPN calculator.

IS THIS, THEN, FORTH, YET?

No, because a defining (eheh) or essential part of FORTH is the ease that you
can define, or even re-define words.

Chuck more famously redefined the number 5:

: 5 5 ;

because he got tired of waiting for the dictionary to search for it before
realizing it was a number.

We're not going there today. Not the number five, and not defining new words,
(nor redefining any words, for that matter). Today, we're adding some simple
stack operations.

SWAP DROP ROT DUP OVER 

Questions on what these words do? CONSULT THE BOOK OF ARMAMENTS!

forth.com/starting-forth/0-starting-forth/

GO!

Now. Forth is a 'concatative'(?) (really?) language. So you string these
words together and you get the Verseilles, ... or something like that.

Forth. I love Forth. My first: MMS ('Miller Microcomputer Services') Forth
was such fun: 16x64 'screens' on my 16x64 TRS-80 Mod I Level 2 4k, and I was
changing the world, one (Forth) word at a time.
--}

import Control.Monad.State
import Y2021.M03.D23.Solution

forth' :: String -> StateT Stack IO ()
forth' = undefined

{--
Now, with the above, and the arithmetic operators from yesterday, answer the
following word-problem, USING FORTH!

If 32% of the students eating at the school cafeteria usually buy bananas, how 
many bananas should be on hand for a crowd of 225? Naturally, we are only 
interested in whole bananas, so we’d like to round off any decimal remainder.

c.f.: https://www.forth.com/starting-forth/5-fixed-point-arithmetic/
--}

wordProblem :: String -> IO ()
wordProblem = undefined

{--
Also, define the 'handy table of Rational Approximations of Various Constants

... IN FORTH!

Handy Table of Rational Approximations to Various Constants:

Number				Approximation		Error
π = 3.141 …			355 / 113		8.5 x 10-8
π = 3.141 …			1068966896 / 340262731	3.0 x 10-18
√2 = 1.414 …			19601 / 13860		1.5 x 10-9
√3 = 1.732 …			18817 / 10864		1.1 x 10-9
e = 2.718 …			28667 / 10546		5.5 x 10-9
√10 = 3.162 …			22936 / 7253		5.7 x 10-9
12√2 = 1.059 …			26797 / 25293		1.0 x 10-9
log(2) / 1.6384 = 0.183 …	2040 / 11103		1.1 x 10-8
ln(2) / 16.384 = 0.042 …	485 / 11464		1.0 x 10-7

ibid.
--}

handyTableOfRationalApproximationsOfVariousConstants :: String -> IO ()
handyTableOfRationalApproximationsOfVariousConstants = undefined
