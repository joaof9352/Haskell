-- * FICHA 1
import Data.Char

module ficha1 where

-- | EX 1

dist :: (Double,Double) -> (Double,Double) -> Double
dist (a,b) (c,d) = sqrt $ (c-a)^2 + (d-b)^2

primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)

multiplo :: Int -> Int -> Bool
multiplo m n = (mod m n) == 0 
            
truncaImpar :: [a] -> [a]
truncaImpar a | mod (length a) 2 == 0 = a
              | otherwise = tail a

max2 :: Double -> Double -> Double
max2 a b | a > b = a
         | otherwise = b

max3:: Double -> Double -> Double-> Double
max3 a b c = max2 a (max2 b c)

-- | EX 2
-- b² - 4ac

nRaizes :: Double -> Double -> Double -> Int
nRaizes a b c | nmr > 0 = 2
              | nmr == 0 = 1
              | otherwise = 0
    where nmr = b^2 - 4*a*c

raizes :: Double -> Double -> Double -> [Double]
raizes a b c | nmr > 0 = [bhaskara,bhaskara2]
             | nmr == 0 = [bhaskara]
             | nmr < 0 = []
  where nmr = b^2 - 4*a*c
        bhaskara = ((-b) + sqrt (nmr))/(2*a)
        bhaskara2 = ((-b) - sqrt (nmr))/(2*a)

-- | EX 4

data Hora = H Int Int deriving(Show,Eq)

testar :: Hora -> Bool
testar (H a b) = c >= 0 && c < 24
  where (H c d) = normalizarHora (H a b)

comparar :: Hora -> Hora -> Bool
comparar (H a b) (H c d) = a > c || b > d
  where (H a1 b1) = normalizarHora (H a b)
        (H c1 d1) = normalizarHora (H c d)

converterh :: Hora -> Int
converterh (H a b) = a * 60 + b

converterm :: Int -> Hora 
converterm a = (H hora minuto)
  where hora = div a 60
        minuto = mod a 60

diferenca :: Hora -> Hora -> (Int, Hora)
diferenca a b = (converterh hf, hf)
  where (H a1 b1) = normalizarHora a 
        (H a2 b2) = normalizarHora b
        (H af bf) = (H (a1-a2) (b1-b2))
        hf = normalizarHora (H af bf)

adicionarMin :: Hora -> Int -> Hora
adicionarMin b min = hf
  where (H a1 b1) = normalizarHora b
        hf = normalizarHora (H (a1) (b1+min))
 
-- | EX 5

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

next :: Semaforo -> Semaforo
next a | a == Verde = Amarelo
       | a == Amarelo = Vermelho
       | a == Vermelho = Verde

stop :: Semaforo -> Bool
stop a = a == Vermelho 


safe :: Semaforo -> Semaforo -> Bool
safe a b = a == Vermelho || b == Vermelho 

-- | EX 6

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)


posx :: Ponto -> Double
posx (Cartesiano b _ ) = abs (b)
posx v = posx . polar2Cart $ v 

posy :: Ponto -> Double
posy (Cartesiano _ b) = abs (b)
posy v = posy . polar2Cart $ v

raio :: Ponto -> Double
raio (Cartesiano a b) = dis
    where
        dis = sqrt (c + d)
        c = a*a
        d = b*b
raio v = raio . polar2Cart $ v

angulo :: Ponto -> Double
angulo (Polar _ b) = b
angulo v = angulo . cart2Polar $ v

distancia :: Ponto -> Ponto -> Double
distancia (Cartesiano a b) (Cartesiano c d) = sqrt (dx + dy)
  where
    dx = (a - c)^2
    dy = (b - d)^2
distancia v1 v2 = distancia (polar2Cart v1) (polar2Cart v2)

-- | EX 7

data Figura = Circulo Ponto Double 
            | Rectangulo Ponto Ponto 
            | Triangulo Ponto Ponto Ponto 
            deriving (Show,Eq)


poligono :: Figura -> Bool
poligono (Circulo p r) = r > 0
poligono (Rectangulo a b) = (polar2Cart a) /= (polar2Cart b)
poligono (Triangulo a b c) | l1 == lm = l1 <= l2 + l3
                           | l2 == lm = l2 <= l1 + l3
                           | l3 == lm = l3 <= l1 + l2
  where l1 = distancia a b
        l2 = distancia b c
        l3 = distancia a c
        lm = max3 l1 l2 l3

vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Rectangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = [(Cartesiano x1 y1),(Cartesiano x1 y2),(Cartesiano x2 y1),(Cartesiano x2 y2)]
vertices (Rectangulo p1 p2) = vertices (Rectangulo (polar2Cart p1) (polar2Cart p2))
vertices (Triangulo p0 p1 p2) = [(polar2Cart p0),(polar2Cart p1),(polar2Cart p2)]

area :: Figura -> Double
area (Triangulo p1 p2 p3) = sqrt $ s*(s-a)*(s-b)*(s-c)
	where a = distancia p1 p2
	      b = distancia p1 p3
	      c = distancia p2 p3
	      s = (a+b+c)/2
area (Circulo _ r) = pi*(r^2)
area (Rectangulo p1@(Cartesiano x1 y1) p4@(Cartesiano x2 y2)) = d1*d2
	where p2 = (Cartesiano x1 y2)
	      p3 = (Cartesiano x2 y1) 
	      d1 = distancia p1 p2
	      d2 = distancia p3 p4
area (Rectangulo p1 p2) = area (Rectangulo (polar2Cart p1) (polar2Cart p2))

perimetro :: Figura -> Double
perimetro (Circulo _ r) = 2*pi*r
perimetro (Rectangulo p1@(Cartesiano x1 y1) p4@(Cartesiano x2 y2)) = 2*d1 + 2*d2
	where p2 = (Cartesiano x1 y2)
	      p3 = (Cartesiano x2 y1) 
	      d1 = distancia p1 p2
	      d2 = distancia p3 p4
perimetro (Rectangulo p1 p2) = perimetro (Rectangulo (polar2Cart p1) (polar2Cart p2))
perimetro (Triangulo p0 p1 p2) = d1 + d2 + d3
	where d1 = distancia p0 p1
	      d2 = distancia p1 p2
	      d3 = distancia p0 p2

--- Funções Auxiliares da Ficha 1 

normalizarHora :: Hora -> Hora
normalizarHora a = converterm . converterh $ a

polar2Cart :: Ponto -> Ponto
polar2Cart c@(Cartesiano x y) = c
polar2Cart (Polar a b) = Cartesiano (a*cos b) (a*sin b)

cart2Polar :: Ponto -> Ponto
cart2Polar c@(Polar a b) = c
cart2Polar (Cartesiano a b) = Polar (a/(cos c)) c
  where
    c = atan(b/a)

