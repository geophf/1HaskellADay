module Y2018.M03.D08.Exercise where

{--
Let's try something different today.

@wtfunctional has a image draw tutorial with the Mandelbrot set:

https://github.com/WhatTheFunctional/ImmutableMandelbrot/blob/master/coloredfractal.hs

(colorized)

Download and compile it, following the hints at 

https://whatthefunctional.wordpress.com/2018/03/03/mandelbrot/

Now:
--}

import Data.Time.Clock

timedMandelbrot :: FilePath -> IO ()
timedMandelbrot outputpng = undefined

{--
Have timedMandelbrot call the mandelbrot set-maker, but then report out how
long it takes to create a mandelbrot set image
--}

smallerMandelbrot :: FilePath -> Int -> Int -> IO ()
smallerMandelbrot outputpng = undefined

{--
Create a mandelbrot set image that is 1/3 the size of the default. How do you
do that? What functions do you call? How long did that take?
--}

{-- BONUS -----------------------------------------------------------------

So, generating a mandelbrot set takes a while. Do you see any obvious bottle-
necks? Do you see any algorithmic improvements that will speed up the image
generation? *hint* profiler

*another hint* The mandelbrot set is an 'embarrassingly parallel' problem.

*COUGH* threads? *COUGH* STM? *COUGH* 

https://hackage.haskell.org/package/stm

One thing I did was to make the iteration/escape-test to be 100 instead of 1000.

What is the trade-off there?

Or there're approaches discussed in wikipedia:

https://en.wikipedia.org/wiki/Mandelbrot_set#Optimizations
--}

{-- BONUS-BONUS -----------------------------------------------------------

We've all seen galleries of 'zooming' into the Mandelbrot set. How would you
do that here?

https://en.wikipedia.org/wiki/Mandelbrot_set#Geometry
--}
