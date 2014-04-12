module HAD.Y2014.M04.D11.Solution where

{- | thirdOfFive

   return the third of five arguments

   No other interest than pointFree

   prop> \(x1, x2, x3, x4, x5) -> thirdOfFive x1 x2 x3 x4 x5  == (x3 :: Int)

   thirdOfFive a b c d e = c
   thirdOfFive a b c d = const c
   thirdOfFive a b c = const $ const c
   thirdOfFive a b c = const . const $ c
   thirdOfFive a b = const . const
   thirdOfFive a = const (const . const)
   thirdOfFive = const (const (const . const))
   thirdOfFive = const $ const (const . const)
   thirdOfFive = const $ const  $ const . const
   thirdOfFive = const . const  $ const . const
-}
thirdOfFive :: a -> b -> c -> d -> e -> c
thirdOfFive = const . const $ const . const . id
