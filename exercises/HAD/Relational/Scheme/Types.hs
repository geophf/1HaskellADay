module Relational.Scheme.Types where

import Data.Maybe (fromJust)
import Control.Applicative ((<|>))

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

-- first, we need some generic maybe-read function-type

mbread :: Read a => String -> Maybe a
mbread s = case [x | (x, t) <- reads s, ("", "") <- lex t] of
   [x] -> Just x
   _   -> Nothing

{--
>>> (mbread "123") :: Maybe Int
Just 123
>>> (mbread "123a") :: Maybe Int
Nothing
--}

replInt :: String -> Maybe Int
replInt = mbread

-- replInt takes a string that can be converted into an Int and returns that Int

{--
>>> replInt "42"
Just 42
--}

replString :: String -> Maybe String
replString = Just

-- replString takes a string and returns that string

{--
>>> replString "Hello, World!"
Just "Hello, World!"
--}

replBool, replBool' :: String -> Maybe Bool
replBool' = mbread
replBool ('#':x:rest) = case x of
   't' -> Just True
   'f' -> Just False
   _   -> Nothing
replBool _            = Nothing

-- replBool takes a string and converts it to its boolean value

{--
>>> replBool' "True"
Just True
>>> replBool "#t"
Just True
>>> replBool "#f"
Just False
--}

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
   show Success = "#s"
   show Fail    = "#u"

instance Read LogicValue where
   readsPrec _ ('#':x:rest) = [(case x of
         's' -> Success
         'u' -> Fail, rest)]
   readsPrec _ _ = []

replLogic :: String -> Maybe LogicValue
replLogic = mbread

-- takes #s or #u as a string and returns the corresponding LogicValue

{--
>>> replLogic "#s"
Just #s
>>> replLogic "#u"
Just #u
--}

{--
Super!

Now, marry all these together. Define a function, replAtom, that takes a string
and returns an atomic value. Note that any string can eventually convert to
a string, can't it. So that means you can have some fun with the Maybe-monad.
--}

data Atom = I Int | B Bool | L LogicValue | S String
   deriving (Eq, Show)

replAtom :: String -> Atom
replAtom val = fromJust
                   ((I <$> replInt val)
                <|> (B <$> replBool val)
                <|> (L <$> replLogic val)
                <|> (S <$> replString val))

{--
>>> replAtom "123"
I 123
>>> replAtom "True"
S "True"               -- n.b. !!!
>>> replAtom "#t"
B True
>>> replAtom "#u"
L #u
>>> replAtom "Goodbye, cruel world!"
S "Goodbye, cruel world!"

Okay, great! We're scanning and parsing atoms! There appears to be a pattern
of application around the (<|>)-operator in the definition of replAtom. Is there
a more succinct definition for that function?
--}
