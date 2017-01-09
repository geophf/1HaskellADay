{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Y2017.M01.D09.Exercise where

import Data.Aeson            -- cabal install aeson
import Data.Aeson.QQ         -- cabal install aeson-qq

import Data.ByteString.Lazy.Char8

{--
Today's Haskell problem is brought to you by quasi-quote by way of @bitemyapp
from the blog entry at:

http://lorepub.com/post/2016-12-17-Haskell-Pitfalls

Quasi-quote is from olden-LISP-eldritch-magic that converted data into code
by mystical macro expansion.

The above statement is entirely, measurable accurate.

Haskell does the EXACT SAME THING!

... not really.

In haskell, we're constrainted by types, in that the result has to be well-
typeable. That's a good thing, but it leads to differences in what we used to
do when we were programming LISP (not Lisp, as 'Lisp' is a new-fangled family of
programming languages, but 'LISP' which sat (snugly) in 4K of memory).

Today we're going to be looking at one aspect of quasi-quotian logic in Haskell,
the JSON-library, Aeson.
--}

-- If you were to have the below definition in Haskell:

john :: Value

{--
john = [aesonQQ|tasmanian devil|]

And then try to compile it, you would get the following error:

Y2017/M01/D09/Exercise.hs:9:8:
    Exception when trying to run compile-time code:
      Error in aesonExp: "txt" (line 1, column 1):
unexpected "a"
expecting "true"
    Code: Language.Haskell.TH.Quote.quoteExp aesonQQ "tasmanian devil"
Failed, modules loaded: none.

This error is actually a good thing. Why? Because you catch this error during
compilation, and not later when you run your program.

So, let's have a well-formed JSON definition of john:
--}

john = [aesonQQ|{"age": 23,
                 "name": "John",
                 "likes": ["linux", "Haskell"]}
        |]

-- That compiles! Great.

-- see also other syntax for aesonQQ at
-- https://github.com/sol/aeson-qq#readme

-- 1. How do you get it to look like JSON. Output john as JSON-text

johnJaeson :: Value -> ByteString
johnJaeson val = undefined

-- 2. access: transfer Value john to a Haskell data type

data Person = Pers { age :: Int, name :: String, likes :: [String] }
  deriving (Eq, Show)

instance FromJSON Person where
   parseJSON = undefined

-- 3. Given the Person of john, what is his age?

ageOf :: Person -> Int
ageOf = undefined

-- 4. Now, take jane, a Person, and convert her to JSON and output as text

jane :: Person
jane = Pers 23 "Jane" []

pers2JSON :: Person -> ByteString
pers2JSON pers = undefined

-- Question: how did you handle that Jane likes nothing?
