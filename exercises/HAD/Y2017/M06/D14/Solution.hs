module Y2017.M06.D14.Solution where

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

-- -- -- -- -- -- -- --

-- Problem 2. 

-- You have the definitions of the functions in Julia (from the referenced 
-- tweet). Define these functions in Haskell.

-- n.b.: Julia is an impure languages: variables actual can have variant
-- values. How do you re-present the Runge-Kutta functions purely in Haskell?

rungeKutta43 :: Floating τ => (τ -> τ -> τ) -> τ -> τ -> τ -> τ -> (τ, τ, τ)
rungeKutta43 f t x τ k1 =

-- τ is Floating, proof: in rungeKutta43Adapt τ = 0.00001 passed as argument here

   let ψ1 = k1

-- t and k are of type τ,  proof: t + 0.5 * τ, τ + 0.5 * k1

       k2 = f (t + 0.5 * τ) (x + τ * 0.5 * k1)
       ψ2 = ψ1 + 2 * k2

-- which also means f is τ -> τ -> τ and x is of type τ

-- ... so now that we know the types of τ, t, x, k1 and f, we need to compute the
-- return type of rungeKutta43

       k3 = f (t + 0.5 * τ) (x + τ * 0.5 * k2)
       ψ3 = ψ2 + 2 * k3

       k4 = f (t + τ) (x + τ * k3)
       ψ4 = x + (1/6) * τ * (ψ3 + k4)

       k5 = f (t + τ) (x + (1/6) * τ * (k1 + 2 * k2 + 2 * k3 + k4))
       ψ5 = x + (1/6) * τ * (ψ3 + k5)

   in  (ψ4, norm (ψ5 - ψ4), k5)

-- so, now we know the return type: (τ, τ, τ)
-- ... and we also know the type of norm: τ -> τ

norm :: Floating τ => τ -> τ
norm = abs -- in this case


-- now for the type of rungeKutta43Adapt. As can be demonstrated, all
-- arguments are Floating τ types. t, tmax, x, k1 are τ as shown above, that
-- leaves the type of tol, which we will show.

rungeKutta43Adapt :: Floating τ => Ord τ => (τ -> τ -> τ) -> τ -> τ -> τ -> τ -> (τ, τ)
rungeKutta43Adapt f t tmax x tol =
   let τ = 0.00001 -- this should be configurable, but okay
       k1 = f t x
       e  = 0
   in  loop τ k1 e f t x tmax tol

loop :: Floating τ => Ord τ => τ -> τ -> τ -> (τ -> τ -> τ) -> τ -> τ -> τ -> τ -> (τ, τ)
loop τ k1 e f t x tmax tol | t >= tmax = (x, e)
                           | otherwise =
   let (ψ, ε, k) = rungeKutta43 f t x τ k1
       (e', x', k', t') =  if ε < tol then (e + ε, ψ, k, t + τ) else (e,x,k,t)
       τ' = minimum [0.9 * τ * (tol / ε) ** (1/3), 3 * τ, tmax -t]
   in  loop τ' k' e' f t' x' tmax tol

-- as e is of type τ, we see that tol is of type τ

-- hint: rungeKutta43Adapt uses rungeKutta43, and these function also use
-- the Julia built-in functions norm and min as well as arithmetic operators
-- so the types can be (eventually) inferred.

-- -- -- -- -- -- -- --

-- Problem 3.

-- solve the following using Runge-Kutta:

f :: Floating a => a -> a -> a
f x y = 3 * exp (-x) - 0.4 * y 

-- where (x,y) = (0,5)
-- what is y when x = 3? What is a good 'step'/Δ to arrive at the solution?

-- We'll go with t = 1.5 here.

{--
>>> rungeKutta43Adapt f 1.5 3 3 0.001
(2.0193332515638356,5.143441429602547e-4)

Now, according to the youtube vid, y = 2.759

SO I DID SOMETHING WRONG! So, okay.
--}