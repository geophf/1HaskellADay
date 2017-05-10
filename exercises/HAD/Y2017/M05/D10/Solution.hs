module Y2017.M05.D10.Solution where

import Prelude hiding ((^))

{--
Another Mensa Genius Quiz-a-Day Book challenge from Dr. Abbie F. Salny, et al:

You've bought your weekly egg supply at the local farm store. The first morning 
you have company for breakfast and use half the eggs plus one-half an egg. The
next morning you use one-half of what's left plus one-half an egg. The third
morning you use one-half of what's left plus one-half an egg, and on the fourth
morning, you're down to one solitary egg, so you make French toast. In all this
cooking, you've never had one-half an egg to carry over to the next day. How
many eggs did you buy originally?
--}

type Eggs = Int

halfPlusOneHalf :: Eggs -> Eggs

-- here I actually go 'backwards' deterministically. That is to say:
-- GIVEN an ending egg-count WHAT is egg-count BEFORE the reduction

-- a backward function, then

halfPlusOneHalf = floor . (2 *) . (+ 0.5) . fromIntegral

eggsBought, frenchToast :: Eggs
eggsBought = (halfPlusOneHalf ^ 3) frenchToast

frenchToast = 1

-- ooh! I'm mixing mathematics and combinators. Ooh! Scary

(^) :: (a -> a) -> Int -> (a -> a)
f ^ 1 = f
f ^ n = f . (f ^ pred n)

-- So, how do we reconcile halves 'inside the black box' of the function with
-- the 'integralness' of the egg-count?

{--
>>> eggsBought 
15
--}
