module Y2020.M07.D30.Solution where

{--
Shoaib and Kate are going on vacation.

We have a database of our team, where the members are:
--}

import Data.Set (Set)
import qualified Data.Set as Set

members, actives :: [String]
members = words ("Roxi Ken Doug Shoaib Victor Robert Morgan Jose "
              ++ "Tony Len Howie Ray Apoorv Nicole")

-- the active members are the team are:

actives = words ("Ken Doug Shoaib Victor Robert Morgan Jose "
              ++ "Tony Len Howie Ray Apoorv Nicole")

-- Question 1: Who are inactive?

inactives :: [String] -> [String] -> [String]
inactives members = Set.toList
                  . Set.difference (Set.fromList members)
                  . Set.fromList

{--
>>> inactives members actives 
["Roxi"]
--}

-- Task: Kate and Shoaib are going on vacation. Write a graph database 
-- command that will remove those members from the active list, following
-- this template:

template :: String -> String
template name = "MATCH (n:Jigsawyer { name: \"" ++ name
             ++ "\" }) REMOVE n:ACTIVE; "

{--
>>> template "Shoaib"
"MATCH (n:Jigsawyer { name: \"Shoaib\" }) REMOVE n:ACTIVE; "
--}

-- use whatever magic to replace "[name]" with the name of the person taking
-- vacay.

-- (also, you could rewrite the template String as a function if that suits
-- you better) (or you could show off your parser-combinator-generator magic)

vacay :: String -> String
vacay = template

{--
>>> vacay "Shoaib"
"MATCH (n:Jigsawyer { name: \"Shoaib\" }) REMOVE n.ACTIVE; "

>>> unlines (map template (words "Shoaib Kate"))
"MATCH (n:Jigsawyer { name: \"Shoaib\" }) REMOVE n:ACTIVE; \n"
++ "MATCH (n:Jigsawyer { name: \"Kate\" }) REMOVE n:ACTIVE; \n"
--}
