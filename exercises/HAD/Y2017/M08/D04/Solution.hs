module Y2017.M08.D04.Solution where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)

-- below import available via 1HaskellADay git repository

import Y2017.M08.D01.Exercise

{--
Yesterday we looked at a rot13 cypher, but that was unsatifactory for a couple
of reasons:

1. it rotted down low on the 13 only. What if you want to rot 12? Would you
have to write a whole new function to rot 12? Why, yes, you would.
2. It was stuck on uppercase ASCII, if you went outside that range, you're
in uppercase ASCII land and descrambling would become confused. Try munging
then demunging pi to 20 places. What would you get? Now switch to Icelandic,
how do you munge then demunge "Verði þér að góðu"

Let's fix this.

How?

Well, when I ran into error rot13 lowercase letters I did some trolling on
twitter (much to the autistic screeching of some purists) and on the interwebz,
and, thanks to SamirTalwar I came across the Cæsar Cipher.

What is the Cæsar Cipher?

well from https://gist.github.com/SamirTalwar/2f93b85c08918d91015d47d45529c82e
we see it is:
--}

caesarCipher :: Int -> [a] -> [(a, a)]
caesarCipher n xs = zip xs (drop n (cycle xs))

-- Cool! How do you use it? Today's Haskell exercise.

-- Given quick from yesterday, imported above, munge and and demunge that
-- string using the caesarCipher 12. Hint: SamirTalwar shows how to use upper
-- and lowercase character sets when munging and demunging.

rotUsing :: String -> String -> Int -> String -> String
rotUsing upper lower n =
   let uppers = caesarCipher n upper
       lowers = caesarCipher n lower
   in  map (\c -> fromMaybe c (lookup c uppers <|> lookup c lowers))

rot :: Int -> String -> String
rot = rotUsing ['A'..'Z'] ['a'..'z']

{--
>>> rot 12 quick 
"    Ftq cguow, ndaiz raj vgybqp ahqd ftq xmlk pas."
--}

-- Hint-hint: how do you demunge caesarCipher 12 text?

{--
>>> rot 14 (rot 12 quick)
"    The quick, brown fox jumped over the lazy dog."
>>> rot 14 (rot 12 quick) == quick
True
--}

-- Now, given the Icelandic alphabet, upper and lower case:

iceHi, iceLo :: String
iceHi = "AÁBDÐEÉFGHIÍJKLMNOÓPRSTUÚVXYÝÞÆÖ"
iceLo = "aábdðeéfghiíjklmnoóprstuúvxyýþæö"

-- munge and demunge 'you're welcome' in Icelandic using caesarCipher 17

youreWelcome :: String
youreWelcome = "Verði þér að góðu"

{--
>>> rotUsing iceHi iceLo 17 youreWelcome 
"Itesy lue os vdsg"
>>> icedMunged = it
>>> length iceHi
32
>>> rotUsing iceHi iceLo 15 icedMunged 
"Ver\240i \254\233r a\240 g\243\240u"
>>> putStrLn $ rotUsing iceHi iceLo 15 icedMunged 
Verði þér að góðu
>>> rotUsing iceHi iceLo 15 icedMunged == youreWelcome 
True
--}
