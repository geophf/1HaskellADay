module Data.Numeral.Boolean where

import Prelude hiding (and, or, not)
import Control.Monad (join)

import Control.Boolean

{--
A solution to the boolean arithmetic problem posted at http://lpaste.net/108358

The meta language:

Given the operators from yesterday (and, or, not, etc), and only one data type, 
Bool, okay: two data types, you can string the bool values together with 
whatever array-like structure you choose (list, deque, vector, array, pick one).

Define the arithmetic operators: addition, subtraction, multiplication,
exponentiation.

Bonus, define division.

The machine language:

Reduce the arithmetic operators to a set of combinations of a single boolean
operator. How are these arithmetic operators defined? How did you manage carry?
--}

-- we use the least-significant-bit-first implementation of binary numbers:

data BNum = B [Bool] deriving Eq

bzero = B []

-- The approach for add:

-- tt: tt = f, tf = t, ft = t, ff = f           -- xor
-- tt for the carry bit: tt = t, otherwise f    -- and

-- splitter = t = t,t, f = f,f

add :: BNum -> BNum -> BNum
add (B a) (B b) = B $ add' a b False

add' :: [Bool] -> [Bool] -> Bool -> [Bool]
add' (h1:t1) (h2:t2) carry = 
   xor (xor h1 h2) carry : add' t1 t2 (or carry (and h1 h2))
add' [] [] carry = appendCarry [] carry
add' list [] carry = add' list [carry] False
add' [] list carry = add' [carry] list False

appendCarry :: [Bool] -> Bool -> [Bool]
appendCarry list False = list
appendCarry list True  = list ++ [True]

{--
*Main> B $ replicate 6 True ~> B63
*Main> add it (B [True, True, False]) ~> B66
*Main> B $ replicate 6 True ~> B63
*Main> add it (B [True, True]) ~> B66
*Main> add (B [True, True]) (B [False, True, True]) ~> B9
*Main> add (B [True, True]) (B [False, True]) ~> B5

Now, add as a set of nand-instructions:

From Control.Boolean:

xor a b = not (equ a b)
   so not = nand True
   so equ a b = or (and a b) (nor a b)
      so or a b = nand (not a) (not b)
      so and a b = not (nand a b)
      so nor a b = not (or a b)

SO:

nand True (nand (nand True a) (nand True b)) 
   where a = and a b
         b = nor a b

SO:

nand True (nand (nand True (nand True (nand a b)))
                (nand True (nand True (nand (nand True a) (nand True b)))))

... and there you have it! YAY!

... um, actually, that's just xor. Let's prove that:
--}

myxor :: Bool -> Bool -> Bool
myxor a b = nand True (nand (nand True (nand True (nand a b)))
                            (nand True (nand True (nand (nand True a)
                                                  (nand True b)))))

-- yep: testBool myxor == testBool xor ~> True

-- Wow.

-- But you get the point, xor reduces to those nands, then iterate.

mult :: BNum -> BNum -> BNum
mult (B a) (B b) = m' a [] []   -- we take the simple shift-add approach
   where m' [] _ sums = foldr add bzero sums
         m' (bit:rest) shift sums =
            m' rest (False:shift) (if bit then B (shift ++ b):sums else sums)

{--
Some multiplies:
*Main> mult (B [True, True]) (B [True, True]) ~> B9
*Main> mult (B [False, False, True]) (B [False, True, True]) ~> B24
--}

-- a sample circuit that does multiplication is at:
-- https://en.wikipedia.org/wiki/Binary_multiplier

----- and helpful printy-like thingies to see what B-numbers we're working with

instance Num BNum where
   fromInteger x = B $ fromInt x
      where fromInt 0 = []
            fromInt x = let (d, m) = x `divMod` 2
                        in nb m : fromInt d
   a + b = add a b
   a * b = mult a b
   abs a = a
   signum a | toNum a == 0 = 0
            | otherwise    = 1
   negate _ = undefined -- some implementations use MSB as sign, ... idk.

instance Show BNum where
   show bools = 'B' : show (toNum bools)

toNum :: BNum -> Integer
toNum (B bools) = t' bools 0
   where t' [] _ = 0
         t' (h : t) n = bn h * 2 ^ n + t' t (succ n)

bn :: Bool -> Integer
bn True = 1
bn False = 0

bc :: Bool -> Char
bc True = '1'
bc False = '0'

nb :: Integer -> Bool
nb 0 = False
nb 1 = True

bits :: BNum -> String
bits (B b) = map bc b

-- Given a set of BNums, scan through each, giving a cross-cutting BNum
-- when one of the BNums is exhausted, returns False thereafter for it

-- you know: your standard matrix transposition problem

transpose :: [BNum] -> [BNum]
transpose [] = []
transpose bnums@(_:_) = t' (map (\(B b) -> b) bnums)
   where t' bs | null (join bs) = []
               | otherwise      = B (heads bs) : t' (tails bs)
         -- we need to regularize the data to the longest BNum
         heads [] = []
         heads ([] : t) = False : heads t
         heads ((h:_) : t) = h  : heads t
         tails [] = []
         tails ([] : t) = [] : tails t
         tails ((h:t):rest) = t : tails rest

{--
So:
*Data.BNum> scan [B [False, False, True], B [True, True], B [True]] ~> [B6,B2,B1]
*Data.BNum> map bits it ~> ["011","010","100"]
--}

