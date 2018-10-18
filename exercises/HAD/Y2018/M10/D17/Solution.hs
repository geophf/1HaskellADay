module Y2018.M10.D17.Solution where

{--
Today. Write a function that takes 2 dates then writes a set of SQL INSERT
statements that enumerates every day between those days into the packet
table.

The packet table structure is

packet table
------------
id,serial
download_dt,timestamp
from_date,date
to_date,date
raw_xml,text
downloaded,text
--}

import Data.Time

type Statement = String

datesBetween :: Day -> Day -> [Statement]
datesBetween from to = if from > to then [] else
   let tomorrow = addDays 1 from in
   ("INSERT INTO packet (from_date,to_date) VALUES ('" ++ show from
    ++ "','" ++ show tomorrow ++ "');"):datesBetween tomorrow to

{--
>>> datesBetween (read "2018-09-10") (read "2018-10-10")
["INSERT INTO packet (from_date,to_date) VALUES ('2018-09-10','2018-09-11');",
 "INSERT INTO packet (from_date,to_date) VALUES ('2018-09-11','2018-09-12');",
 "INSERT INTO packet (from_date,to_date) VALUES ('2018-09-12','2018-09-13');",
 ...]
--}
