{-# LANGUAGE ViewPatterns #-}

module Y2021.M03.D29.Solution where

{--
We wish to enumerate a number of items in our ... 'Fruits Basket,' ... eheh

https://en.wikipedia.org/wiki/Fruits_Basket

Not that Fruits Basket.

And we wish to not embarrass ourselves with the output, in English.

That is to say: we're defining handling plurality and connectives.
--}

import Data.Char (toLower)
import Data.List (group, sort)

data Fruit = Apple | Banana | Pear | Lime | Tomato
   deriving (Eq, Ord, Show)

-- because Tomato is a FRUIT! :<

plural :: [a] -> String
plural [_] = ""
plural _   = "s"

-- plural [Apple, Apple] = "s"
-- plural [Apple] = ""

connector :: [a] -> [b] -> String
connector (length -> xs) (length -> ys) = c' (xs * ys)

-- btw, George Boole is awesome. His boolean arithmetic reduces this problem
-- in his original notation (*) stood for (&&) where 0 is False and any other
-- number is True (e.g.: Monoid Product)

c' :: Int -> String
c' 0 = ""
c' _ = " and"

{--
>>> connector [] [Apple]
""
>>> connector [Pear] []
""
>>> connector [Banana] [Lime]
" and"
--}

{--
Now, write a function, fruitsBasket, that takes a set of various fruits,
groups them, then prints out the contents of those fruits, e.g.:

>>> fruitsBasket oneFruits
"In my fruits basket, I have 2 apples."

>>> fruitsBasket noFruits
"In my fruits basket, I have no fruits."

>>> fruitsBasket twoFruits
"In my fruits basket, I have 1 apple and 2 pears."
--}

fruitsBasket :: [Fruit] -> String
fruitsBasket = (fb' <*> map length) . group . sort

fb' :: [[Fruit]] -> [Int] -> String
fb' fruits counts = intro ++ outro (zip fruits counts) (sum counts) ++ fullStop

-- More George Boole-logic: sum stands for (||) (actually: foldr (||) False)
-- where 0 is False and any other number is True (i.e.: Monoid Sum)

intro, fullStop :: String
intro = "In my fruits basket, I have"

fullStop = "."

outro :: [([Fruit], Int)] -> Int -> String
outro _ 0 = " no fruits"
outro fruits _ = counter fruits

counter :: [([Fruit], Int)] -> String
counter [(f, l)] = counts f l
counter ((f1, l1):f2:fs) =
   counts f1 l1 ++ connector f1 (fst f2) ++ counter (f2:fs)

counts :: [Fruit] -> Int -> String
counts [] _ = ""
counts fs@(f:_) l = ' ':show l ++ " " ++ downcase f ++ plural fs

downcase :: Fruit -> String
downcase = ((:) . toLower . head <*> tail) . show

-- here are your test-scenarios:

oneFruits, noFruits, twoFruits :: [Fruit]
oneFruits = [Apple, Apple]
noFruits = []
twoFruits = [Pear, Apple, Pear]

{--
... and, ... a final flourish:

>>> fruitsBasket [Apple, Pear, Lime, Apple]
"In my fruits basket, I have 2 apples and 1 pear and 1 lime."
--}
