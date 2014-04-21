module HAD.Y2014.M04.D16.Solution where

{- | reverseMap 
   No definition, look to types
-}
reverseMap :: [a -> b] -> a -> [b]
reverseMap = sequence

-- Or, longer
reverseMap' :: [a -> b] -> a -> [b]
reverseMap' = flip $ map . flip ($)
