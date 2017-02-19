module Analytics.Math.Combinatorics where

-- Where you want to get all factorial and stuff

factorial :: Integer -> Rational
factorial = toRational . product . enumFromTo 1

choose :: Integer -> Integer -> Rational
choose n k = factorial n / (factorial k * factorial (n - k))
