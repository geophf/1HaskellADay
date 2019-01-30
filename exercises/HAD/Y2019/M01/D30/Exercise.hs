module Y2019.M01.D30.Exercise where

import Data.Time

import Y2019.M01.D28.Exercise
import Y2019.M01.D29.Exercise (convert2HTML)

{--
Let's take the two sets of articles from Y2019.M01.D28.Exercise.articleFiles
and merge them into one set of articles, sorted by date. For the result
we want just the artId and text, and text being the result of 
Y2019.M01.D29.Exercise.convert2HTML
--}

import Data.Map (Map)

type ArticleId = String
type HTMLText = String

articlesByDate :: [Article] -> Map Day (Map ArticleId HTMLText)
articlesByDate arts = undefined

-- Takes the set of JSON-like articles and HTML-encoded articles and returns
-- all the articles HTML-encoded by article-id by date.

