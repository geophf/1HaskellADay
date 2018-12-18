module Y2018.M12.D18.Solution where

{--
Read in 'diffs.txt' in this directory, then compute the final value after 
applying all these values from a starting value of 0.
--}

exDir, diffs :: FilePath
exDir = "Y2018/M12/D18/"
diffs = "diffs.txt"

result :: FilePath -> IO Integer
result = fmap (sum . map readNum . words) . readFile

readNum :: String -> Integer
readNum (s:n) = (if s == '-' then negate else id) (read n)
