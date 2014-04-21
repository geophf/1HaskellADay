module HAD.Y2014.M04.D09.Exercise where

-- $setup
-- >>> import Data.List

data Foo = Foo {x :: Int, y :: String, z :: String}
  deriving (Read, Show, Eq)

{- | orderXYZ
   Order Foo by x then by y and then by z
   
   prop> sort xs == (map x . orderXYZ . map (\v -> Foo v  "y" "z")) xs
   prop> sort xs == (map y . orderXYZ . map (\v -> Foo 42  v  "z")) xs
   prop> sort xs == (map z . orderXYZ . map (\v -> Foo 42 "y"  v )) xs
-}
orderXYZ :: [Foo] -> [Foo]
orderXYZ = undefined
