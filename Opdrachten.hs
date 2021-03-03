module Opdrachten where

-- Opgaves (O)

-- determinant a b c = square b - 4 * a * c

-- O1.2
aantalOpl a b c
  | d > 0 = 3
  | d == 0 = 2
  | d < 0 = 1
  where
    d = b ^ 2 - 4 * a * c

-- O1.3
fac 0 = 1
fac x = x * fac (x -1)

fac' x
  | x == 0 = 1
  | otherwise = x * fac (x -1)

-- O1.4
-- een voorbeeld is logischerwijs de tweede afgeleide en ..

-- O1.5
lijst = 1 : [1] -- Voor het toevoegen van een element

lijst2 :: [Integer] -- type definition
lijst2 = [1, 2] ++ [1] -- Voor het samenvoegen van lijsten

-- O1.6 a
som xs
  | xs == [] = 0
  --   of null xs
  | otherwise = head xs + som (tail xs)

-- O1.6 b
som' [] = 0
som' (x : xs) = x + som xs

-- O1.7 a
eenVanDe xs
  | null xs = False
  | otherwise = head xs || eenVanDe (tail xs)

-- O1.7 b
eenVanDe' [] = False
eenVanDe' (x : xs) = x || eenVanDe' xs

-- O1.7 c
eenVanDe'' xs = foldr (||) False xs

-- O1.8
-- map :: (a -> b) -> [a] -> [b]

-- O1.9 a
-- quot :: Integral a => a -> a -> a
-- div :: Integral a => a -> a -> a
-- quot en div hebben een type variabele a uit klasse Integral

-- O1.9 b
-- De typen die gebruikt kunnen worden door quot en div zijn dus Int en Integer,
-- want die vallen onder de klasse Integral

-- O1.9 c
-- 7.3 :: Fractional p => p

-- O1.9 d
-- Double en Float

-- O1.10
-- Door www.haskell.org/haskellwiki/Monomorphism_restriction
-- werkt dit niet bij mijn versie van ghci
-- Foldable t => (a -> b -> b) -> b -> t a -> b
som'' :: Num b => [b] -> b
som'' = foldr (+) 0

-- O1.11
oneven :: Integral a => a -> Bool
oneven = not . even

-- om te testen of het inderdaad hetzelfde is
oneven' :: Integral x => x -> Bool
oneven' x = (not . even) x

-- O1.12
-- map (^2) [1,2,3,..,10]

-- O1.13
-- f (x, y) betekent dat f een tupel verwacht.
-- f is in haskell een functie die een parameter heeft namelijk de tupel.

-- O1.14
-- a) (plus 3) (plus 4 5) == plus 3 (plus 4 5)
-- b) sqrt (3) + (sqrt 4) == sqrt 3 + sqrt 4
-- c) (a -> b) -> (c -> d) == (a -> b) -> c -> d

-- O1.15
-- lang verhaal, zie uitwerkingen

-- O1.16
data Boom a
  = Leeg
  | Splits a (Boom a) (Boom a)

somBoom :: Boom Int -> Int
somBoom Leeg = 0
somBoom (Splits x lBoom rBoom) = x + somBoom lBoom + somBoom rBoom

-- Zelftoets (ZT)

-- ZT1 a) Een functie in een functie
-- ZT1 b) Een functie waarbij minder argumenten aan een functie gegeven worden
-- ZT1 c) Een functie die verschillende type argumenten kan verwerken
-- ZT1 d) Een functie die bij verschillende type ook verschillende functionaliteit toepast
-- ZT1 e) Een functie die gebruikt wordt om een data type op te bouwen

-- ZT2 a) Type is alleen een soort alias en een data declaratie is echt een nieuw 'type' dat gemaakt kan worden etc.
-- Een data declaratie kan vervolgens ook in de type definitie van een functie gebruikt worden
-- ZT2 b) een operator is ook een functie het verschil is alleen de plaats waar deze staat.
-- Een operator maakt gebruik van infix notatie, terwijl een functie gebruik maakt van prefix notatie.

-- ZT3 a)
productLijst :: Num p => [p] -> p
productLijst xs
  | null xs = 1
  | otherwise = head xs * productLijst (tail xs)

-- ZT3 b)
productLijst' :: Num p => [p] -> p
productLijst' [] = 1
productLijst' (x : xs) = x * (productLijst xs)

-- ZT3 c)
productLijst'' :: Num p => [p] -> p
-- productLijst'' xs = foldr (*) 1 xs
productLijst'' = foldr (*) 1

-- ZT4
eersteDieIs :: (p -> Bool) -> [p] -> p
eersteDieIs p (x : xs)
  | p x = x
  | otherwise = eersteDieIs p xs

-- ZT5
boomElementen :: Boom p -> [p]
boomElementen Leeg = []
boomElementen (Splits x lBoom rBoom) = boomElementen lBoom ++ [x] ++ boomElementen rBoom