module Analytics.Math.Combinatorics where

-- Where you want to get all factorial and stuff

factorial :: Integer -> Integer
factorial = product . enumFromTo 1

choose :: Integer -> Integer -> Integer
choose n k = factorial n / (factorial k * factorial (n - k))
