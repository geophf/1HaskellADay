module Y2021.M04.D26.Solution where

{--
Remember when everybody was Kung Fu fighting?

HUWAAA!

Well, since today is my BIRTHDAY!

YAAAAAAA!

Let's convert typed values to TLAs and TLAs back to typed values.

"What's a TLA?" you ask.

"Ah!" I reply: "You don't work in the Gubmint, I see... and it shows."

TLA, n, acro.: "Three Letter Acronym."

So you have these birthday values.
--}

import Control.Arrow ((&&&))

import Data.Char (isUpper)
import qualified Data.Map as Map

data BirthdayStuphfen =
     BirthdayCostcoCheesecake
   | HuggablePlattyPus
   | ChickenRamenSoup
   | BirthdayCardfromDaughters
   | PiranhafistEatingFace
   | ChocolateMoussewithCoffees
   | WhirledPeasonEarth
      deriving (Eq, Ord, Enum, Show)

-- convert BirthdayStuphfen values to TLAs

data TLA = TLA String
   deriving (Eq, Ord, Show)

toTLA :: BirthdayStuphfen -> TLA
toTLA = TLA . filter isUpper . show

{--
>>> toTLA BirthdayCostcoCheesecake
TLA "BCC"

>>> toTLA ChickenRamenSoup
TLA "CRS"

... you get the idea.
--}

{-- BONUS -------------------------------------------------------

Now, here's the fun part:

convert from a TLA to a BirthdayStuphfen-value.
--}

fromTLA :: TLA -> Maybe BirthdayStuphfen
fromTLA =
   flip Map.lookup
        (Map.fromList $ map (toTLA &&& id) [BirthdayCostcoCheesecake .. ])

{--
>>> fromTLA (TLA "WPE")
Just WhirlePeasonEarth

>>> fromTLA (TLA "xyzzy")
Nothing

... you thought I was going to write "Nothing happens," but I didn't. AHA! :<

... until now. Oh, well. :,(
--}
