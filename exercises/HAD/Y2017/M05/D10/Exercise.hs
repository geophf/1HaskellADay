module Y2017.M05.D10.Exercise where

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
halfPlusOneHalf eggs = undefined

eggsBought :: Eggs
eggsBought = undefined

-- So, how do we reconcile halves 'inside the black box' of the function with
-- the 'integralness' of the egg-count?
