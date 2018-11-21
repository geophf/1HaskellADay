module Y2018.M11.D21.Solution where

{--
From @fermatslibrary 

https://twitter.com/fermatslibrary/status/1061619809669074946

"A curious identity involving π and e:"

1 / (π² + 1) + 1 / (4π² + 1) + 1 / (9π² + 1) + 1 / (16π² + 1) + ... = 1 / (e² - 1)

How many terms do you have to go out for that equation to be accurate within
0.1? 0.01? 0.001? 0.0001?
--}

pie :: Int -> Double
pie n = abs (1 / (exp 2 + 1) - p n 1)

-- pie returns the value of the sum of the first n π² terms minus the e term

p :: Int -> Double -> Double
p 0 _ = 0
p n x = p (pred n) (succ x) + 1 / (x*x * pi * pi + 1)

howManyTerms :: Double -> Int
howManyTerms = hmt 1

hmt :: Int -> Double -> Int
hmt x err = if pie x < err then x else hmt (succ x) err

{--
>>> p 1 1
9.199966835037524e-2
>>> p 2 1
0.11670419138223287

>>> pie 1
2.720325367174231e-2

>>> howManyTerms 0.1
1
>>> howManyTerms 0.01
2
>>> howManyTerms 0.001

[loops]
--}
