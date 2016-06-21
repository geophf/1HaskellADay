module Y2016.M06.D20.Solution where

{--
SO! My daughter is working on solving quadratic equations, so, GUESS WHAT WE'RE
DOING TODAY?

Just guess.

So, the standard form of the quadratic equation is:

ax^2 + bx + c = 0

And is solved with:

x = (-b +/- sqrt (b^2 - 4ac)) / 2a

BECAUSE SCIENCE!

Now we have complex number problems here and multiple solutions problems here,
but, eh! That's why you're coders. If we had no problems, you'd be painting
still lifes of daisies or eggplants.

So, you're welcome!

GIVEN: a set of quadratic equations, represent them in standard form (I guess)
(I mean, if that helps you), and then solve each equation for all solutions.
--}

data QuadraticEq = Q { a, b, c :: Float } deriving Show

solver :: QuadraticEq -> [Float]
solver q = [formula] <*> [(-),(+)] <*> [q]

formula :: (Float -> Float -> Float) -> QuadraticEq -> Float
formula f q = (f (negate $ b q) (sqrt (b q ^ 2 - 4 * a q * c q))) / (2 * a q)

-- the equations are given as (a, b, c) in of the standard quadratic equations

equs :: [(Float, Float, Float)]
equs = [(6,7,-3), (10, -1, -2), (4,3,-27), (15,-26,-21), (21,-12,1), (8,25,13)]

q :: (Float, Float, Float) -> QuadraticEq
q (a,b,c) = Q a b c 

{--
*Y2016.M06.D20.Solution> map (solver . q) equs 
[[-1.5,0.33333334],[-0.4,0.5],[-3.0,2.25],[-0.6,2.3333333],
 [0.1012865,0.4701421],[-2.466052,-0.658948]]
--}
