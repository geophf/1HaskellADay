module Y2018.M11.D27.Exercise where 

{--
Eight identically sized squares of paper have been placed on top of one another as follows. Which one is at the bottom?

 ______________
| 7    |   8   |
|______________|
|   |      | 2 |
|---|  1   |---|
| 6 |      |   |
|----------| 5 |
|   3  | 4 |   |
 --------------
--}

data Paper = P1 | P2 | P3 | P4 | P5 | P6 | P7 | P8
   deriving (Eq, Ord, Enum, Show)

data OnTopOf a b = AsYouDeclare

onTopOf :: Paper -> Paper -> OnTopOf Paper Paper
onTopOf px py = undefined

relations :: [OnTopOf Paper Paper]
relations = undefined

bottom :: [OnTopOf Paper Paper] -> [Paper]
bottom rels = undefined
