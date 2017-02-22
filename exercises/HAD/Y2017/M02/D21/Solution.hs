module Y2017.M02.D21.Solution where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map

import Network.HTTP.Conduit

-- below imports available via 1HaskellADay git repository

import Analytics.Math.Combinatorics (fibonacci)
import Control.Logic.Frege ((<<-))

import Y2017.M02.D13.Solution (parseMetadata)

{--
Mortal Fibonacci Rabbits solved by 5142 as of February 20th, 2017

http://rosalind.info/problems/fibd/

Problem

Recall the definition of the Fibonacci numbers from “Rabbits and Recurrence 
Relations”, which followed the recurrence relation F(n)=F(n−1)+F(n−2) and 
assumed that each pair of rabbits reaches maturity in one month and produces a 
single pair of offspring (one male, one female) each subsequent month.

Our aim is to somehow modify this recurrence relation to achieve a dynamic 
programming solution in the case that all rabbits die out after a fixed number 
of months. See Figure 4 for a depiction of a rabbit tree in which rabbits live 
for three months (meaning that they reproduce only twice before dying).

Given: Positive integers n <= 100 and m <= 20

Return: The total number of pairs of rabbits that will remain after the n-th 
month if all rabbits live for m months.
--}

sample :: String
sample = "6 3"

result :: Integer
result = 4

-- this function may help:

fibo :: Integer -> Integer
fibo = fibonacci

{-- SIDE EXCURSION --------------------------------------------------

First of all, recall that recur was defined as follows:

recur :: Integer -> Integer -> Integer
recur 1 k = 1
recur 2 k = 1
recur n k = recur (pred (pred n)) k * k + recur (pred n) k

but recall that dynamic programming allows us to speed up this computation:
--}

recur :: Integer -> Integer -> Integer
recur = recurring [1,1]

recurring :: [Integer] -> Integer -> Integer -> Integer
recurring (a:b:_) n k | n < 1     = 0
                      | n < 3     = a
                      | otherwise = recurring [b * k + a,a] (pred n) k

{--
>>> recur 5 3
19
>>> recur 35 2 
11453246123

...in no time

Note that I drop 'older' elements of the recurrence list: after they are 
computed, they are no longer needed and discarded, so the recurrence 
relationship is now efficient in both time and space, whereas the original
doubly recursive algorithm is efficient in neither.
--}

-- AND NOW BACK TO SOLVING THE PROBLEM ... ------------------------------

-- okay, really? Is the best way to define this is by OOP? Really?

data Rabbit = Born | Breeding Int
   deriving (Eq, Ord, Show)

nRabbitsAtGenerationWithMortality :: Int -> Int -> Integer
nRabbitsAtGenerationWithMortality gen =
   sum . Map.elems . go (Map.singleton Born 1) (pred gen)

go :: Map Rabbit Integer -> Int -> Int -> Map Rabbit Integer
go bunnehz 0 _ = bunnehz
go bunnehz n mort = 

-- first age all the born bunnehz
   let (borns:breedings) = Map.toList bunnehz
       matures     = (Breeding mort, snd borns)

-- next have the breeding bunnehz reproduce

       babies      = (Born, sum (map snd breedings))

-- now we age the breeders

       newbreeders = age (matures:breedings) in

-- and with that, we progress

   go (Map.fromList (babies:newbreeders)) (pred n) mort

age :: [(Rabbit, Integer)] -> [(Rabbit, Integer)]
age [] = []
age ((Breeding 1, _):rest) = age rest  -- dead bunnehz can just DIIIEEEEEE!!!
age ((Breeding n, m):rest) = (Breeding (pred n), m):age rest

{--
>>> go (Map.singleton Born 1) 5 3
fromList [(Born,2),(Breeding 1,1),(Breeding 2,1)]

>>> nRabbitsAtGenerationWithMortality 6 3
4
>>> nRabbitsAtGenerationWithMortality 6 3 == result
True
---}

{-- BONUS -----------------------------------------------------------------

Solve for some larger numbers in the file at this directory at rosalind_fibd.txt
or at the URL:
--}

url :: FilePath
url = "https://raw.githubusercontent.com/geophf/1HaskellADay/master/exercises/HAD/Y2017/M02/D21/rosalind_fibd.txt"

{--
>>> fmap (uncurry nRabbitsAtGenerationWithMortality . parseMetadata . BL.unpack) $ simpleHttp url
1773872490291705971

Okay, so I don't know what-all this has to do with the fibonacci series, but
I did get to write a cute, little state machine with sets of automata with a 
very small memory footprint, given the large number of state-machines, ...

And when I say 'large,' I mean:

>>> go (Map.singleton Born 1) 89 19
fromList [(Born,1096293440841652511),(Breeding 1,189899394348161),
          (Breeding 2,307248963220840),(Breeding 3,497115463292131),
          (Breeding 4,804311205003240),(Breeding 5,1301340558206700),
          (Breeding 6,2105512440830541),(Breeding 7,3406627581485117),
          (Breeding 8,5511775306517487),(Breeding 9,8917812793699023),
          (Breeding 10,14428633353290644),(Breeding 11,23344901407976056),
          (Breeding 12,37771035440708002),(Breeding 13,61111893056690658),
          (Breeding 14,98876385817777603),(Breeding 15,159977693103317404),
          (Breeding 16,258836951603642742),(Breeding 17,418786933452018939),
          (Breeding 18,677579049450053460)]

... so there's that.

... And people wonder why there're rabbit-problems in places such as Australia 
and Iceland, where there are no natural predators of rabbits.

Those rabbits just keep breeding like, ... well, ... RABBITS!
--}
