module Opdrachten where

-- determinant a b c = square b - 4 * a * c

-- opgave 1.2
aantalOpl a b c
  | d > 0 = 3
  | d == 0 = 2
  | d < 0 = 1
  where
    d = b ^ 2 - 4 * a * c

-- opgave 1.3
fac 0 = 1
fac x = x * fac (x -1)

fac' x
  | x == 0 = 1
  | otherwise = x * fac (x -1)

-- opgave 1.4
-- een voorbeeld is logischerwijs de tweede afgeleide en ..

-- opgave 1.5
lijst = 1 : [1] -- Voor het toevoegen van een element

lijst2 :: [Integer] -- type definition
lijst2 = [1, 2] ++ [1] -- Voor het samenvoegen van lijsten

-- opgave 1.6 a
som xs
  | xs == [] = 0
  --   of null xs
  | otherwise = head xs + som (tail xs)

-- opgave 1.6 b
som' [] = 0
som' (x : xs) = x + som xs

-- opgave 1.7 a
eenVanDe xs
  | null xs = False
  | otherwise = head xs || eenVanDe (tail xs)

-- opgave 1.7 b
eenVanDe' [] = False
eenVanDe' (x : xs) = x || eenVanDe' xs

-- opgave 1.7 c
eenVanDe'' xs = foldr (||) False xs

-- opgave 1.8
-- map :: (a -> b) -> [a] -> [b]

-- opgave 1.9 a
-- quot :: Integral a => a -> a -> a
-- div :: Integral a => a -> a -> a
-- quot en div hebben een type variabele a uit klasse Integral

-- opgave 1.9 b
-- De typen die gebruikt kunnen worden door quot en div zijn dus Int en Integer,
-- want die vallen onder de klasse Integral

-- opgave 1.9 c
-- 7.3 :: Fractional p => p

-- opgave 1.9 d
-- Double en Float

-- opgave 1.10
-- Door www.haskell.org/haskellwiki/Monomorphism_restriction
-- werkt dit niet bij mijn versie van ghci
-- Foldable t => (a -> b -> b) -> b -> t a -> b
som'' :: Num b => [b] -> b
som'' = foldr (+) 0

-- opgave 1.11
oneven :: Integral a => a -> Bool
oneven = not . even

-- om te testen of het inderdaad hetzelfde is
oneven' :: Integral x => x -> Bool
oneven' x = (not . even) x

-- opgave 1.12
-- map (^2) [1,2,3,..,10]

-- opgave 1.13
-- f (x, y) betekent dat f een tupel verwacht.
-- f is in haskell een functie die een parameter heeft namelijk de tupel.

-- opgave 1.14
-- a) (plus 3) (plus 4 5) == plus 3 (plus 4 5)
-- b) sqrt (3) + (sqrt 4) == sqrt 3 + sqrt 4
-- c) (a -> b) -> (c -> d) == (a -> b) -> c -> d

-- opgave 1.15
