module Y2021.M03.D23.Exercise where

{-- 
HOWDY! So, what shall we do today?

I would like to implement FORTH, but that's just me. Did you know that in
FORTH pi is simply defined as:

: pi 355 113 */ ;

(c.f.: https://www.forth.com/resources/forth-programming-language/)

I LOVE IT! What's even more amazing is that Chuck Moore developed Forth
as a telescope guidance control language, and that definition of pi got the
telescope to point to any star. 355 113 */ was accurate enough for astronomy!

YAY!

Okay, so: let's do this.

The FORTH prompt is ('was'? 'is'?) "OK> " and you enter your FORTH and you
fly.

A number is pushed on the stack and an operator reads from the stack.

Let's implement an RPN with * / + - and . where '.' prints pops the stack and
prints that value.
--}

import Control.Monad.State

data Stack = Stack [Int]
   deriving (Eq, Ord, Show)

forth :: String -> StateT Stack IO ()
forth = undefined

repl :: IO ()   -- for "Read Evaluate Print Loop"
repl = undefined

{--
>>> repl
OK> 3 4 + .
7

OK> 
--}
