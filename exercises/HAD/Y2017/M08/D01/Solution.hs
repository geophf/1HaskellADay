{-# LANGUAGE TupleSections #-}

module Y2017.M08.D01.Solution where

import Control.Arrow (first)
import Control.Applicative ((<|>))
import Data.Char
import Data.Maybe
import Data.Monoid ((<>))
import Data.Tuple (swap)

-- below import available via 1HaskellADay git repository

import Control.DList
-- import Data.Stream -- if you can't stand the thought of streams-as-lists

{--
Okay, today we're going to take a string, munging the words, but leaving the 
word-separators intact. Whoa.

Well, this is rather a tall order! I suppose I could go about this by rewriting
words with the following definition in mind:

a word is an alphanumeric string
a separator is a string of non-alphanumerics

And then look at the file as a set of lines with words and separators.

This looks, then, like it will be a two-pass munger. The first pass to 
accumulate just the words, then the next pass is to replace the words between
the separators... 

This is all very stream-y.

First of all, we need to slurp in a word from the stream ... WHOOPS!

WHAT IS A STREAM?

First of all, we need to convert our file (String) to a stream.
--}

type Stream a = [a]

{--
Okay, that's all kinds of wrong, and the academics are going into seizures
right now, but just point them to the definition of String, and tell them
that string processing is faster than 'ByteString' (whatever the hell that is)
and has a more-compact representation, and then watch them lose their fucking
minds.

Yeah. I went there.
--}

strS :: String -> Stream Char
strS = id

-- convert to a stream the following quick string

quick :: String
quick = "    The quick, brown fox jumps over the lazy dog."

-- question and hint: how do you handle EOF or EOString?
-- n.b.: the terminal character in H-Tables is NOT '\0'. Think about it.

{--
>>> strS quick 
"    The quick, brown fox jumps over the lazy dog."

Now that you've got strings as streams, now we are going to parse the stream,
feeling all Uustalu, Vene and Pardo-like.

The next word in a stream from the stream-head is the list of unbroken alphanums
--}

nextWord :: Stream Char -> (Maybe String, Stream Char)
nextWord list@(s:str) =
   if isAlphaNum s then (first pure (nw id (dl' s) str)) else (Nothing, list)

{--
>>> nextWord "Hi there!"
(Just "Hi"," there!")
--}

-- nw finds the next char in the word or the boundary per the boolean directive

-- I use DList (difference lists) so I can append to the end in constant time

nw :: (Bool -> Bool) -> DList Char -> Stream Char -> (String, Stream Char)
nw f acc [] = ret acc []
nw f acc list@(h:t) =
   if f (isAlphaNum h) then nw f (acc <| h) t else ret acc list

ret :: DList a -> b -> ([a], b)
ret dl = (dlToList dl,)

-- find the next word in the stream of quick.

-- The next word-boundary in a stream is the list of unbroken non-alphanums

nextBound :: Stream Char -> (Maybe String, Stream Char)
nextBound list@(h:t) =
   if not (isAlphaNum h) then first pure (nw not (dl' h) t) else (Nothing, list)

-- n.b. the pattern that nextWord uses is duplicated by nextBound. Can this
-- pattern be generalized?

{--
>>> nextBound quick 
(Just "    ","The quick, brown fox jumps over the lazy dog.")
--}

-- The terminus is when the string in the inexhaustible stream is exhausted.

termin :: Stream Char -> Bool
termin = null

-- (rhymes with 'wermin' a la "The Whole Nine Yards")

-- Okay, we have our parsers, now we need to stitch them back together into
-- a (primitive) grammar

data Token = Word String | Boundary String
   deriving (Eq, Show)

-- We know the next token is going to be a string or a word-boundary, so let's 
-- get that next token!

nextToken :: Stream Char -> (Maybe Token, Stream Char)
nextToken [] = (Nothing, [])
nextToken list@(_:_) =
   fall ((first Word <$> lift (nextWord list))
     <|> (first Boundary <$> lift (nextBound list)))

-- we need to lift (Maybe a, b) to Maybe (a,b)

lift :: Applicative f => (f a, b) -> f (a, b)
lift = fmap swap . sequenceA . swap

fall :: Maybe (a, b) -> (Maybe a, b)
fall (Just (a, b)) = (Just a, b)

{--
>>> nextToken quick 
(Just (Boundary "    "),"The quick, brown fox jumped over the lazy dog.")
--}

-- Parse an input string into arranged words and boundaries

parse :: String -> [Token]
parse = p . first dl' . nextToken . strS

p :: (DList (Maybe Token), Stream Char) -> [Token]
p (acc, str) = if termin str then catMaybes (dlToList acc)
               else p (first (acc <|) (nextToken str))

{--
>>> parse quick 
[Boundary "    ",Word "The",Boundary " ",Word "quick",Boundary ", ",
 Word "brown",Boundary " ",Word "fox",Boundary " ",Word "jumps",Boundary " ",
 Word "over",Boundary " ",Word "the",Boundary " ",Word "lazy",Boundary " ",
 Word "dog",Boundary "."]
--}

-- Now it gets fun. Given a munging function, ... let's say it's rot13, ...

rot13 :: Char -> Char
rot13 x = chr ((ord (toUpper x) - ord 'A' + 13) `mod` 26 + ord 'A')

-- (of course, rot13 only makes sense with ALL CAP STRINGS!)

-- rot13 is just one example of a munging function, another example could be an
-- 'English to Greek'-munger, or another scheme which we may develop.

-- munge the words, but don't munge the boundaries (hint: you'll have to
-- expand the rot13 function to work over an entire string, not just 1 char.

munge :: (String -> String) -> String -> String
munge = id

mbApp :: (String -> String) -> Token -> Token
mbApp f (Word x) = Word (f x)
mbApp _ b@(Boundary x) = b

{--
>>> map (mbApp (munge (map rot13))) (parse quick)
[Boundary "    ",Word "GUR",Boundary " ",Word "DHVPX",Boundary ", ",
 Word "OEBJA",Boundary " ",Word "SBK",Boundary " ",Word "WHZCF",Boundary " ",
 Word "BIRE",Boundary " ",Word "GUR",Boundary " ",Word "YNML",Boundary " ",
 Word "QBT",Boundary "."]
--}

-- then stitch t ogether the parsed line back into a string of munged words:

stitch :: (String -> String) -> [Token] -> String
stitch dict = concatMap (detokenize . mbApp dict)

detokenize :: Token -> String
detokenize (Word x) = x
detokenize (Boundary x) = x

{--
>>> stitch (map rot13) (parse quick)
"    GUR DHVPX, OEBJA SBK WHZCF BIRE GUR YNML QBT."
>>> cypherTxt = it
--}

-- Now demunging munged text is very much like munging plaintext. Given a
-- demunging function, in this case also rot13, parse, then pass the demunge
-- function to the parsed line to stitch back together the original message.

-- Is the demunged message of the munged message the same as the original 
-- message? If not, why not?

{--
>>> stitch (map rot13) (parse cypherTxt)
"    THE QUICK, BROWN FOX JUMPS OVER THE LAZY DOG."

It's not the same, as it's all upper case.
--}
