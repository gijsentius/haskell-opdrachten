module Opdrachten where

-- opdracht 1.5
lijst = 1 : [1] -- Voor het toevoegen van een element

lijst2 :: [Integer] -- type definition
lijst2 = [1, 2] ++ [1] -- Voor het samenvoegen van lijsten

-- opdracht 1.6 a
som xs
  | xs == [] = 0
  --   of null xs
  | otherwise = head xs + som (tail xs)

-- opdracht 1.6 b
som' [] = 0
som' (x : xs) = x + som xs