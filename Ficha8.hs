module Ficha8 where

data Frac = F Integer Integer 

-- Mandar para 930613494

--data Ordering = LT | EQ | GT

normaliza :: Frac -> Frac
normaliza (F a b) | b < 0 = normaliza (F (negate a) (negate b))
                  | otherwise = F (div a c) (div b c)
    where c = mdc (abs a) b


mdc :: Integer -> Integer -> Integer
mdc a b | a == b = a
        | a > b = mdc (a-b) b
        | a < b = mdc a (b-a)

-- a * b = mmc a b * mdc a b

mmc :: Integer -> Integer -> Integer
mmc a b = div (a * b) (mdc a b)


instance Eq Frac where 
    (==) (F a b) (F c d) = (div a b) == (div c d)
    (/=) (F a b) (F c d) = (div a b) /= (div c d)

instance Ord Frac where
    compare (F a b) (F c d) | (div a b) == (div c d) = EQ
                            | (div a b) < (div c d) = LT
                            | otherwise = GT

instance Show Frac where
    show (F a b) = show a ++ "/" ++ show b

instance Num Frac where
    (+) (F a b) (F c d) = F (a2+c2) b2
        where (F a2 b2) = normaliza (F (a2*d) (b2*d))
              (F c2 d2) = normaliza (F (c2*b) (d2*b))
    (-) (F a b) (F c d) = (F (a2-c2) b2)
        where (F a2 b2) = normaliza (F (a2*d) (b2*d))
              (F c2 d2) = normaliza (F (c2*b) (d2*b))
    (*) (F a b) (F c d) = F (a*c) (b*d)
    negate (F a b) = F (negate a) b
    abs (F a b) = F (abs a) (abs b)
    signum (F a b) | a * b < 0 = -1
                   | otherwise = 1
    fromInteger a = (F a 1)

maioresdobro :: Frac -> [Frac] -> [Frac]
maioresdobro (F a b) ((F c d):t) | 2*(F a b) < (F c d) = (F c d):maioresdobro (F a b) t
                                 | otherwise = maioresdobro (F a b) t
maioresdobro _ [] = []

-- * Exercício 2
data Exp a = Const a
             | Simetrico (Exp a)
             | Mais (Exp a) (Exp a)
             | Menos (Exp a) (Exp a)
             | Mult (Exp a) (Exp a)

--instance Show (Exp m) where
--    show (Const s) = show s
--    show (Simetrico s) = "-" ++ show s
--    show (Mais a b) = show a ++ "+" ++ show b
--    show (Menos a b) = show a ++ "-" ++ show b
--    show (Mult a b) = show a ++ "*" ++ show b

