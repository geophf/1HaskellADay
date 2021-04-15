module Y2021.M04.D14.Solution where

{--
So, Haskell has this, like, type-system, see?

SHOCKER! I know.

But, types aren't manager-readable (that's a term now).

Today's Haskell problem.

Convert a typed value to a manager-readable value.
--}

import Data.Char (isUpper)
import Data.List (intercalate, break)

import Control.DList   -- a la difference lists, a la Prolog, ...functionalized

data KungFu = KungFuFighting
            | FastAsLightning
            | ALittleBitFrightening
            | ExpertTiming
            | Huwaa
            | TheyCallMeBruce
            | CrouchingTigerHiddenDragon
            | BuddhaPalm
            | CraneTechnique
            | AiKiDo
            | DrunkenBoxing
   deriving (Eq, Show)

-- change, e.g.: KungFuFighting to "Kung Fu Fighting"

deCamelCaseMartialArtsFightingStylezYo :: KungFu -> String
deCamelCaseMartialArtsFightingStylezYo = deCamelCase

{--
>>> deCamelCaseMartialArtsFightingStylezYo KungFuFighting 
"Kung Fu Fighting"
>>> deCamelCaseMartialArtsFightingStylezYo DrunkenBoxing 
"Drunken Boxing"
--}

{-- BONUS -------------------------------------------------------

Now, generalize this for any (showable) typed-value.
--}

data Panda = MooShu
           | MooGuGaiPan
           | GeneralTsoChicken
           | SweetAndSpicyProk
           | ThousandYearEgg
           | StinkyTofu
           | BobaTea
           | GulabJamoon
           | CandiedYams
           | PurpleYamIceCream
           | ChocoPie
           | PekingDuck
           | DrunkenNoodles
   deriving (Eq, Show)

deCamelCase :: Show a => a -> String
deCamelCase = intercalate " " . flip dcc' emptyDL . show

dcc' :: String -> DList String -> [String]
dcc' "" ans = dlToList ans
dcc' (h:t) acc =
   let (w1,w2) = break isUpper t
   in  dcc' w2 (acc <| (h:w1))

{--
>>> deCamelCase MooGuGaiPan 
"Moo Gu Gai Pan"
>>> deCamelCase DrunkenBoxing 
"Drunken Boxing"
>>> deCamelCase DrunkenNoodles 
"Drunken Noodles"

... question: Do you eat Drunken Noodles when you're Drunken Boxing?
--}
