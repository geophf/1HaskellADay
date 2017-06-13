module Y2017.M06.D14.Exercise where

{--
So, matthieu bulté (@matthieubulte) threw it down yesterday with #JuliaLang
saying the 4-2 Adaptive Runge-Kutta method in Julia is easy and pretty.

reference: https://twitter.com/matthieubulte/status/874350943575437312

And so it is.

In Haskell, it's easier and prettier.

Prove or disprove my assertion.

The Runge-Kutta functions in Julia are defined in the tweet above (and also
captured the runge-kutta-4-3.jpg image captured here).
--}

-- Problem 1: Manual type inference and refinement

-- (but not Refined types, as that means something else entirely!)

-- Below are the arguments to both functions. What are the types of these
-- functions? Be as specific as possible in your type definitions, so, for
-- example, the argument f has a more specific type than 'a' as you see in
-- its applications that it is some kind of function-type, also τ, t, etc, are
-- more specific than 'a' as we see τ has a numeric value, and t has ordering
-- and additive properties... to τ! ... HMMMM!

-- Show your work (that is to say: prove the types! AHA!)

rungeKutta43 f t x τ k1 = undefined

rungeKutta43Adapt f t tmax x tol = undefined

-- hint: rungeKutta43Adapt uses rungeKutta43, and these functions also use
-- the Julia built-in functions norm and min as well as arithmetic operators
-- so the types can be (eventually) inferred.

-- -- -- -- -- -- -- --

-- Problem 2. 

-- You have the definitions of the functions in Julia (from the referenced 
-- tweet). Define these functions in Haskell.

-- n.b.: Julia is an impure languages: variables actual can have variant
-- values. How do you re-present the Runge-Kutta functions purely in Haskell?

-- -- -- -- -- -- -- --

-- Problem 3.

-- solve the following using Runge-Kutta:

f :: Floating a => a -> a -> a
f x y = 3 * exp (-x) - 0.4 * y 

-- where (x,y) = (0,5)
-- what is y when x = 3? What is a good 'step'/Δ to arrive at the solution?