import Data.Char

-- * EX 2

dobros :: [Float] -> [Float]
dobros = map (*2)

numOcorre :: Char -> String -> Int
numOcorre a [] = 0
numOcorre a (h:t) | a == h = 1 + (numOcorre a t) 
				  | otherwise = numOcorre a t

positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) | h <= 0 = False
                | otherwise = positivos t

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) | h < 0 = h + (somaNeg t)
			  | otherwise = somaNeg t

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt a@(h:t) | length a <= 3 = a
          		| otherwise = tresUlt t

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos (h:t) = (snd h):segundos t

nosPrimeiros :: Eq a => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False 
nosPrimeiros c (h:t) = c == fst (h) || nosPrimeiros c t

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):l) = (a + sumX, b + sumY, c + sumZ)
	where (sumX, sumY, sumZ) = sumTriplos l

-- * EX 3
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) | elem h ['0'..'9'] = h:soDigitos t
                | otherwise = soDigitos t

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) | elem h ['a'..'z'] = 1 + minusculas t
                 | otherwise = minusculas t

nums :: String -> [Int]
nums [] = []
nums (h:t) | elem h ['0'..'9'] = (digitToInt h):nums t
           | otherwise = nums t

-- * EX 4
type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta a (h:t) | a == (snd h) = 1 + (conta a t)
              | otherwise = conta a t

grau :: Polinomio -> Int
grau (h:t:ts) | snd h >= snd t = grau (h:ts)
              | otherwise = grau (t:ts)
grau (h:[]) = snd h

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau a (h:t) | a == snd h = h:selgrau a t
                | otherwise = selgrau a t

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv (h:t) | snd h - 1 < 0 = deriv t
            | otherwise = ((fst h * (fromIntegral (snd h))),(snd h - 1)):deriv t

calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula a (h:t) = (x*(a^y))+calcula a t 
	where (x,y) = (fst h,snd h)

simp :: Polinomio -> Polinomio 
simp [] = []
simp (h:t) | fst h == 0 = simp t
           | otherwise = h:simp t

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (h,t) (x:xs) = (h*a,t+b):mult (h,t) xs
	where (a,b) = (fst x, snd x)

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza (h:t) = mete h (normaliza t)

mete :: Monomio -> Polinomio -> Polinomio
mete a [] = [a]
mete (a,b) (h:t) | b == snd h = (a+fst h,b):t
                 | otherwise = h:mete (a,b) t

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = simp (normaliza (p1 ++ p2))

produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto (h:t) p = mult h p ++ produto t p

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (h:t) = (inserta h (ordena t))

inserta :: Monomio -> Polinomio -> Polinomio
inserta a [] = [a]
inserta (x,y) (a:as) | y >= snd a = a:inserta (x,y) as
                     | otherwise = (x,y):a:as

equiv :: Polinomio -> Polinomio -> Bool
equiv a b = n1 == n2
	where o1 = ordena a
	      o2 = ordena b
	      n1 = normaliza o1
	      n2 = normaliza o2