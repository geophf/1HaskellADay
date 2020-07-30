module Y2020.M07.D30.Exercise where

{--
Shoaib and Kate are going on vacation.

We have a database of our team, where the members are:
--}

members, actives :: [String]
members = words ("Roxi Ken Doug Shoaib Victor Robert Morgan Jose "
              ++ "Tony Len Howie Ray Apoorv Nicole")

-- the active members are the team are:

actives = words ("Ken Doug Shoaib Victor Robert Morgan Jose "
              ++ "Tony Len Howie Ray Apoorv Nicole")

-- Question 1: Who are inactive?

inactives :: [String] -> [String] -> [String]
inactives members actives = undefined

-- Task: Kate and Shoaib are going on vacation. Write a graph database 
-- command that will remove those members from the active list, following
-- this template:

template :: String
template = "MATCH (n:Jigsawyer { name: \"[name]\" }) REMOVE n:ACTIVE; "

-- use whatever magic to replace "[name]" with the name of the person taking
-- vacay.

-- (also, you could rewrite the template String as a function if that suits
-- you better) (or you could show off your parser-combinator-generator magic)

vacay :: String -> String -> String
vacay template member = undefined

{--
>>> vacay template "Shoaib"
"MATCH (n:Jigsawyer { name: \"Shoaib\" }) REMOVE n.ACTIVE; "
--}
