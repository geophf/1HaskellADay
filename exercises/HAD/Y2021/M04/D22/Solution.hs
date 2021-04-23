module Y2021.M04.D22.Solution where

{--
What's the opposite of Kung Fu Fighting?

I think, actually, we're not looking for the opposite of super-cool fighting-
styles, but their dual.

So, what's the dual of Kung Fu Fighting?

That's a much easier question to ask, and to answer.
--}

import Data.Maybe (mapMaybe)

import Y2021.M04.D08.Solution (readMaybe)

data WuShu = ShaolinSoccer
           | KungFuHustle
           | FruitsBasket
  deriving (Eq, Ord, Show, Read)

{--
Here's the problem:

>>> (read "ShaolinSoccer") :: WuShu
ShaolinSoccer

... works find. However, if you recall from a previous exercise

... (Y2021.M04.D14.Exercise) ...

We write out these types in human-readable forms, AND they're in our data-store
in that format, SO, GIVEN THAT, how do we read a value that's been human-
readable-i-tized?

(That's a word now).
--}

dataStore :: [String]
dataStore = ["Shaolin Soccer", "Fruits Basket", "Kung Fu Hustle"]

fetchFightingStyles :: [String] -> [WuShu]
fetchFightingStyles = mapMaybe (readMaybe . concat . words)

{--
>>> fetchFightingStyles dataStore
[ShaolinSoccer,FruitsBasket,KungFuHustle]
--}

{-- BONUS -------------------------------------------------------

How do you handle erroneous values? How would you convert the following and
get as much information out as possible without failing?
--}

dataStoreBonus :: [String]
dataStoreBonus =
   dataStore
      ++ ["Fruits Basket", "Basket o' Fruits", "Shaolin Soccer", "Orange"]

{--
>>> fetchFightingStyles dataStoreBonus 
[ShaolinSoccer,FruitsBasket,KungFuHustle,FruitsBasket,ShaolinSoccer]
--}
