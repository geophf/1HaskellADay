module Y2021.M04.D14.Exercise where

{--
So, Haskell has this, like, type-system, see?

SHOCKER! I know.

But, types aren't manager-readable (that's a term now).

Today's Haskell problem.

Convert a typed value to a manager-readable value.
--}

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
deCamelCaseMartialArtsFightingStylezYo kf = undefined

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
deCamelCase showaNoKamay = undefined
