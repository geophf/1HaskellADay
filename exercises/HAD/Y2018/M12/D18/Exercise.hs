module Y2018.M12.D18.Exercise where

{--
Read in 'diffs.txt' in this directory, then compute the final value after 
applying all these values from a starting value of 0.
--}

exDir, diffs :: FilePath
exDir = "Y2018/M12/D18/"
diffs = "diffs.txt"

result :: FilePath -> IO Integer
result diffsFile = undefined

-- looking at the format of the numbers in diffs.txt, you MAY have to write
-- your own parser, ... or does the read function just work here?
