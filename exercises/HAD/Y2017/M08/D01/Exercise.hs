module Y2017.M08.D01.Exercise where

import Data.Char

-- below import available via 1HaskellADay git repository

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
strS str = undefined

-- convert to a stream the following quick string

quick :: String
quick = "    The quick, brown fox jumped over the lazy dog."

-- question and hint: how do you handle EOF or EOString?
-- n.b.: the terminal character in H-Tables is NOT '\0'. Think about it.

{--
Now that you've got strings as streams, now we are going to parse the stream,
feeling all Uustalu, Vene and Pardo-like.

The next word in a stream from the stream-head is the list of unbroken alphanums
--}

nextWord :: Stream Char -> (Maybe String, Stream Char)
nextWord str = undefined

-- find the next word in the stream of quick.

-- The next word-boundary in a stream is the list of unbroken non-alphanums

nextBound :: Stream Char -> (Maybe String, Stream Char)
nextBound str = undefined

-- The terminus is when the string in the inexhaustible stream is exhausted.

termin :: Stream Char -> Bool
termin str = undefined

-- (rhymes with 'wermin' a la "The Whole Nine Yards")

-- Okay, we have our parsers, now we need to stitch them back together into
-- a (primitive) grammar

data Token = Word String | Boundary String
   deriving (Eq, Show)

-- We know the next token is going to be a string or a word-boundary, so let's 
-- get that next token!

nextToken :: Stream Char -> (Token, Stream Char)
nextToken str = undefined

-- Parse an input string into arranged words and boundaries

parse :: String -> [Token]
parse str = undefined

-- Now it gets fun. Given a munging function, ... let's say it's rot13, ...

rot13 :: Char -> Char
rot13 x = chr ((ord x - ord 'A' + 13) `mod` 26 + ord 'A')

-- (of course, rot13 only makes sense with ALL CAP STRINGS!)

-- rot13 is just one example of a munging function, another example could be an
-- 'English to Greek'-munger, or another scheme which we may develop.

-- munge the words, but don't munge the boundaries (hint: you'll have to
-- expand the rot13 function to work over an entire string, not just 1 char.

munge :: (String -> String) -> String -> String
munge munf word = undefined

-- then stitch together the parsed line back into a string of munged words:

stitch :: (String -> String) -> [Token] -> String
stitch dict parsedLine = undefined

-- Now demunging munged text is very much like munging plaintext. Given a
-- demunging function, in this case also rot13, parse, then pass the demunge
-- function to the parsed line to stitch back together the original message.

-- Is the demunged message of the munged message the same as the original 
-- message? If not, why not?
