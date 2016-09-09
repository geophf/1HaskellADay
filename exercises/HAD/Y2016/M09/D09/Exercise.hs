module Y2016.M09.D09.Exercise where

import Data.Map (Map)

{--
Now for something completely different ...

So some related cryptoarithmetic:
--}

sums :: [(String, String, Int)]
sums = [("FAT", "CAT", 904), ("STORE","CAKE",60836), ("EACH", "ROSE", 7839)]

-- solve these sums

solver :: (String, String, Int) -> Map Char Int -> Map Char Int
solver sum domain = undefined

-- solving all three will map the letter to the digits [0..9]

-- arrange the character in their digit-value order. What word do you get?

arrange :: Map Char Int -> String
arrange = undefined
