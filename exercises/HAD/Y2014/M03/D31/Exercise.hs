module HAD.Y2014.M03.D31.Exercise where

-- $setup
-- >>> import Control.Applicative

-- | emptyIndices List the indices of a list of maybes that contains Nothing
--
-- prop> (all (isNothing) .) . map . (!!) <*> emptyIndices $ xs
--
emptyIndices :: [Maybe a] -> [Int]
emptyIndices = undefined
