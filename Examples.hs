module Examples where

import Data.Ratio

-- Operator priority test
between x = 2 < x && x < 8

--  import test
breuk x y = x % y

-- Lengte van een lijst
lengte lijst
  | null lijst = 0
  | otherwise = 1 + lengte (tail lijst)