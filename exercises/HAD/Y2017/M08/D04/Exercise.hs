module Y2017.M08.D04.Exercise where

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

-- Hint-hint: how do you demunge caesarCipher 12 text?

-- Now, given the Icelandic alphabet, upper and lower case:

iceHi, iceLo :: String
iceHi = "AÁBDÐEÉFGHIÍJKLMNOÓPRSTUÚVXYÝÞÆÖ"
iceLo = "aábdðeéfghiíjklmnoóprstuúvxyýþæö"

-- munge and demunge 'you're welcome' in Icelandic using caesarCipher 17

youreWelcome :: String
youreWelcome = "Verði þér að góðu"
