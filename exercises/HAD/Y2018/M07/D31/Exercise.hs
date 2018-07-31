module Y2018.M07.D31.Exercise where

{--
KAKURO-DAY!

Today we're going to be solving arithmetic problems then solving mutually-
dependent arithmetic problems. Some of these problems have more than one
solution, so we have to solve these problems knowing this constraint.
--}

import Data.Array

-- CONTEXT: For x1, x2, ..., xn in [1..9]:
--          all xs must be mutually-different numbers

type Domain = [Int]

domain :: [Int]
domain = [1..9]

-- solve x1 + x2 = 3, return the answer in an Array

data XS = X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9
   deriving (Eq, Ord, Show, Enum, Bounded, Ix)

data Context = Ctx Domain [Array XS Int]

x1_plus_x2_equals_3 :: [Context] -> [Context]
x1_plus_x2_equals_3 ctxn= undefined

-- Of course, the initial context is that all variables are unbound.
-- ... how do you represent that?

-- Also note that the context can vary based on the outcome. How to model that?

-- what are the values that you arrive at for x1 and x2?

-- Okay, given the solution above, solve x1 + x3 = 5

x1_plus_x3_equals_5 :: [Context] -> [Context]
x1_plus_x3_equals_5 ctx = undefined 

-- what are the values you arrive at for x1 and x3? Are any values 'settled'?
-- That is to say: have we bound x1 or x3 (or both) to a single value?

-- And, given the above, solve x2 + x4 = 3

x2_plus_x4_equals_3 :: [Context] -> [Context]
x2_plus_x4_equals_3 ctxn = undefined

-- What are the values you arrive at for x2 and x4? What variables are settled?
