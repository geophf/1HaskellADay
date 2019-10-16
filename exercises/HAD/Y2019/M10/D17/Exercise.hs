module Y2019.M10.D17.Exercise where

{--
This came up whilst waiting on car repairs.

You can buy 1 bagel of any kind (onion, combination, cinnamon-raisin, plain, ...
for 0.79 USD.

OR

You can buy a pack of 5 bagels of one kind (of any kind) for 3 USD.

OR

You can choose six as a set of any kind of bagel for 3.49 USD.

Which is the best deal?

Write a Haskell program that computes the price per bagel for each of the 
options.
--}

import Data.Map (Map)
import Data.Map as Map

data Deal = ChooseOne | FivePack | SelectSix
   deriving (Eq, Ord, Show)

type USD = Float

totalPrice :: Map Deal USD
totalPrice = Map.fromList [(ChooseOne, 0.79), (FivePack, 3.0), (SelectSix, 3.49)]

pricePerBagel :: Map Deal USD -> Map Deal USD
pricePerBagel storePrices = undefined
