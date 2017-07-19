module Y2017.M07.D19.Exercise where

{--
So, I'm working with a guy who says 'But I like Lisp!'

Okay!

So, data and programs, programs and data. One of the neat properties of Lisp
is that everything is a value, and every value is an atom or a list.

So, here we go. We'll be using the Reasoned Schemer by Friedman, Byrd and 
Kiselyov as a basis to build a little Lisper ... in Haskell.

We'll see how this goes.

Looking at chapter 1, it looks like we need to define some values, but we also 
must create the context in which these values can be ... eheh ... evaluated, 
that is to say, have meaning in the context.

We're talking the Lisp read-eval-print loop, or REPL for short.

So, let's start by evaluating atoms that have a 1-to-1 corresponding value in
Haskell.
--}

replInt :: String -> Maybe Int
replInt val = undefined

-- replInt takes a string that can be converted into an Int and returns that Int

replString :: String -> Maybe String
replString val = undefined

-- replString takes a string and returns that string

replBool :: String -> Maybe Bool
replBool val = undefined

-- replBool takes a string and converts it to its boolean value

{--
Fine. We've got all possible atomic data values from the above, right?

I joke.

Okay, There are atomic values in Scheme that have their own implied meanings
such as, #t and #f for True and False (see above) and, in the Reasoned Schemer:
#s and #u for successful and unsuccessful, or the values success and fail in
'logic scheme,' whatever that is.

Write an interpreter that takes a pound-value as a string and returns that
value, ... while you're at it, rewrite replBool to return a Bool-value but
taking input strings as #t or #f.
--}

data LogicValue = Success | Fail
   deriving Eq

instance Show LogicValue where
   show val = undefined

replLogic :: String -> Maybe LogicValue
replLogic val = undefined

-- takes #s or #u as a string and returns the corresponding LogicValue

{--
Super!

Now, marry all these togeter. Define a function, replAtom, that takes a string
and returns an atomic value. Note that any string can eventually convert to
a string, can't it. So that means you can have some fun with the Maybe-monad.
--}

data Atom = I Int | B Bool | L LogicValue | S String
   deriving (Eq, Show)

replAtom :: String -> Atom
replAtom val = undefined
