module HAD.Y2014.M03.D31.Solution where

import Data.List (findIndices, elemIndices)
import Control.Monad (void)
import Data.Maybe (isNothing)

-- $setup
-- >>> import Control.Applicative
-- >>> import Data.Maybe (isNothing)

-- | emptyIndices List the indices of a list of maybes that contains Nothing
--
-- prop> (all (isNothing) .) . map . (!!) <*> emptyIndices $ xs
--
emptyIndices :: [Maybe a] -> [Int]
emptyIndices = findIndices isNothing

-- | emptyIndices' List the indices of a list of maybes that contains Nothing
--
-- prop> (all (isNothing) .) . map . (!!) <*> emptyIndices $ xs
--
emptyIndices' :: [Maybe a] -> [Int]
emptyIndices' = elemIndices Nothing . map void

-- | emptyIndices' List the indices of a list of maybes that contains Nothing
--
-- prop> (all (isNothing) .) . map . (!!) <*> emptyIndices $ xs
--
emptyIndices'' :: [Maybe a] -> [Int]
emptyIndices'' = map fst . filter (isNothing . snd) . zip [0..]
