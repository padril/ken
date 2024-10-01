module Util (splitBy) where

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy v = filter (not . null) . foldr f [[]]
    where
        f x acc | x == v = []:acc
        f x (h:t) = (x:h):t

