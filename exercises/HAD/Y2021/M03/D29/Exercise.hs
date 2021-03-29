module Y2021.M03.D29.Exercise where

{--
We wish to enumerate a number of items in our ... 'Fruits Basket,' ... eheh

https://en.wikipedia.org/wiki/Fruits_Basket

Not that Fruits Basket.

And we wish to not embarrass ourselves with the output, in English.

That is to say: we're defining handling plurality and connectives.
--}

data Fruit = Apple | Banana | Pear | Lime | Tomato
   deriving (Eq, Ord, Show)

-- because Tomato is a FRUIT! :<

plural :: [a] -> String
plural = undefined

-- plural [Apple, Apple] = "s"
-- plural [Apple] = ""

connector :: [a] -> [b] -> String
connector xs ys = undefined

-- connector [] [Apple, ...] = ""
-- connector [Pear, ...] [] = ""
-- connector [Banana, ...] [Lime, ...] = " and"

{--
Now, write a function, fruitsBasket, that takes a set of various fruits,
groups them, then prints out the contents of those fruits, e.g.:

>>> fruitsBasket [Apple, Apple]
"In my fruits basket, I have 2 apples."

>>> fruitsBasket []
"In my fruits basket, I have no fruits."

>>> fruitsBasket [Pear, Apple, Pear]
"In my fruits basket, I have 1 apple and 2 pears."
--}

fruitsBasket :: [Fruit] -> String
fruitsBasket = undefined

-- here are your test-scenarios:

oneFruits, noFruits, twoFruits :: [Fruit]
oneFruits = [Apple, Apple]
noFruits = []
twoFruits = [Pear, Apple, Pear]
