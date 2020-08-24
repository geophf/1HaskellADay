module Y2020.M08.D24.Exercise where

{--
I've been thinking about cosine similarities...

https://en.wikipedia.org/wiki/Cosine_similarity

... but before we get our cosine similarity-game on, let's limber up a bit.

Let's play, a little, with cosines today.

Today's Haskell problem: multiple two numbers using the cosine-method and
compare those results with, you know, the actual multiplication of those 
numbers.

The cosine-method:

https://en.wikipedia.org/wiki/Prosthaphaeresis

Or, to steal the text, blatantly, because: who follows the links?

For example, say we want to multiply 105 and 720. Following the steps:

1. Scale down: Shift the decimal point three places to the left in each. We get

cos a = 0.105 and cos b = 0.720

2. Inverse cosine: cos 84 degrees is about 0.105 and cos 44 degrees is 
   about 0.720

3. Sum and difference: 84+44=128 and 84-44=40

4. Average the cosines: (cos 128 degrees + cos 40 degrees) / 2 is about 
   (-0.616+0.766) / 2, or 0.075.

5. Scale up: For each of 105 and 720, we shifted the decimal point three places 
   to the left, so in the answer we shift six places to the right. The result 
   is 75,000. This is very close to the actual product, 75,600 (a percent error
   of ≈0.8%).

ta-dah!

Write this algorithm. Compare the results of it to the actual products.
--}

cosineMultiply :: Int -> Int -> Int
cosineMultiply a b = undefined

numbersToMultiply :: [(Int, Int)]
numbersToMultiply = [(204,588), (44,77), (551,564),(805,499),(607,130)]

{--
>>> map (uncurry (*)) numbersToMultiply 
[119952,3388,310764,401695,78910]

... so you have your basis to get going, then!
--}
