module Control.Automata.Cellular where

import Control.Comonad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Data.Numeral.Boolean
import Data.SnowFlake
import Data.Stream
import Data.Universe

{-- 
For comonadic cellular automata, I followed the article by sigfpe
"Evaluating cellular automata is comonadic" at
http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html

... and who is that interloper who suggested the rule 30 implementation? ;/
--}

rule30 :: Enum a => U a -> a
rule30 (U (a:_) b (c:_)) = bitFrom (r' (bitof a b c))
   where r' 111 = 0
         r' 110 = 0
         r' 101 = 0
         r' 100 = 1    -- 16
         r' 011 = 1    --  8
         r' 010 = 1    --  4
         r' 001 = 1    --  2 = 30
         r' 000 = 0
         bitof x y z = 100 * t x + 10 * t y + t z
         t = fromEnum
         bitFrom = toEnum

-- so the generic rule converts the rule number to a stream of bits and then
-- processes them according to the cells above. Let's do that:

type Rule a = U a -> a

genRule :: Enum a => Int -> Rule a
genRule x =
   let (B bits) = fromInteger (toInteger x)
       ateBit   = map fromEnum (take 8 (bits ++ repeat False))
       schema   = Map.fromList $ zip [0, 1, 10, 11, 100, 101, 110, 111] ateBit
   in  \ (U (a:_) b (c:_)) -> toEnum $ fromJust $ Map.lookup (bitOf a b c) schema
       where bitOf x y z = 100 * t x + 10 * t y + t z
             t           = fromEnum

runRule :: Enum a => Rule a -> U a -> Stream (U a) -- a ... stream? of Universes?
runRule rule seed = 
   let stream = seed :< (stream =>> (\strm -> extract strm =>> rule)) in stream
 -- okay, that was too easy. Srsly!

oohPretty :: (Enum a, Show a) => Rule a -> U a -> Int -> Int -> IO ()
oohPretty rule seed length width =
   o' length (runRule rule seed) (flip showCompact width)
      where o' 0 _ _ = return ()
            o' n (row :< rows) s = putStrLn (s row) >> o' (pred n) rows s

toInt :: Enum a => [a] -> Int
toInt [] = 0
toInt list@(h : t) = fst $ foldr (\bit (sum, idx) ->
                                       (sum + (fromEnum bit) * idx, idx * 2))
                                 (0, 1) list

{--
Usage pattern is of the form, e.g.:  cells =>> rule30

For Cellular Automata rules, see:

https://en.wikipedia.org/wiki/Elementary_cellular_automaton

Ooh! Pretty pictures!

Particularly:

https://en.wikipedia.org/wiki/Rule_110 ... which is Turing-complete!

And Rule 30, which is 'the' random number generator.

try: oohPretty (genRule 60) seed 40 20

where seed is defined as: U (repeat Snow) Flake (repeat Snow) (I'm not kidding)
--}

seed :: U SnowFlake
seed = U (repeat Snow) Flake (repeat Snow)

{--
You can generate a Sierpinski triange with:
*Control.Automata.Cellular> oohPretty (genRule 90) seed 16 20 ~>

U ____________________*_______________________________________
U ___________________*_*______________________________________
U __________________*___*_____________________________________
U _________________*_*_*_*____________________________________
U ________________*_______*___________________________________
U _______________*_*_____*_*__________________________________
U ______________*___*___*___*_________________________________
U _____________*_*_*_*_*_*_*_*________________________________
U ____________*_______________*_______________________________
U ___________*_*_____________*_*______________________________
U __________*___*___________*___*_____________________________
U _________*_*_*_*_________*_*_*_*____________________________
U ________*_______*_______*_______*___________________________
U _______*_*_____*_*_____*_*_____*_*__________________________
U ______*___*___*___*___*___*___*___*_________________________
U _____*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*________________________
--}
