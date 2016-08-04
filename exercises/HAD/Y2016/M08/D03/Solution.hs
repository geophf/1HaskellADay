module Y2016.M08.D03.Solution where

import qualified Data.Set as Set

import Control.Logic.Frege ((<<-))
import Y2016.M08.D01.Solution (summer)

{--
http://www.menneske.no/kakuro/eng/
cheatSheet 27 4 ~> [[3,7,8,9],[4,6,8,9],[5,6,7,9]]

given summer 27 4 gives me this as its value:

[[3,7,8,9],[3,7,9,8],[3,8,7,9],[3,8,9,7],... 72 values in the returned list

One approach is to take a trip to the Set category and back, like so:
--}

cheatSheet :: Int -> Int -> [[Int]]
cheatSheet =
   map Set.toList . Set.toList . Set.fromList . map Set.fromList <<- summer

-- *Y2016.M08.D03.Solution> cheatSheet 27 4 ~> [[3,7,8,9],[4,6,8,9],[5,6,7,9]]
