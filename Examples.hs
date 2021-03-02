module Examples where

import Data.Ratio

--  import test
breuk x y = x % y

-- Werkt niet
positief x
  | x < 0 = - x
  | x >= 0 = x

-- Voorbeeld van een hogere orde functie (een functie in een functie)
diff f =    f'
    where   f' x  = (f (x-h) - f x) / h
            h   = 0.0001

-- Voorbeeld van een recursieve implementatie van machtsverheffen
power x 0 = 1
power x y = x * power x (y-1)

power' x n
    | n == 0 = 1
    | otherwise = x * power' x (n-1)

-- Operator priority test
between x = 2 < x && x < 8

-- Lengte van een lijst
lengte lijst
  | null lijst = 0
  | otherwise = 1 + lengte (tail lijst)

-- test met foldr
-- VS code geeft hier een hint die niet werkt
eenVanDe = (||) False

eenVanDe' :: Foldable t => t Bool -> Bool
eenVanDe' xs = foldr (||) False xs

-- Test met map
verdubbel x = map (*2) x

-- samenstellen van functies met . voorbeeld
noteven n = (not . even) n