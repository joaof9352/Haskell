module Ficha7 where

data ExpInt = Const Int
              | Simetrico ExpInt
              | Mais ExpInt ExpInt
              | Menos ExpInt ExpInt
              | Mult ExpInt ExpInt

max2 :: Int -> Int -> Int
max2 a b | a > b = a
         | otherwise = b


-- * Exercício 1

calcula :: ExpInt -> Int
calcula (Const a) = a
calcula (Simetrico a) = - (calcula a)
calcula (Mais e1 e2) = calcula e1 + calcula e2
calcula (Menos e1 e2) = calcula e1 - calcula e2
calcula (Mult e1 e2) = calcula e1 * calcula e2

-- Fazer em casa
infixa :: ExpInt -> String
infixa (Const a) = undefined

-- * Exercício 2
exr :: RTree Int
exr = R 2 [b,c,d]
    where 
        b = R 3 [] 
        c = R 4 [R 5 [],R 6 []]
        d = R 7 [R 8 [],R 9 [],R 10 [],R 11 []]

data RTree a = R a [RTree a] deriving Show

soma :: Num a => RTree a -> a
soma (R x xs) = x + sum (map soma xs)

altura :: RTree a -> Int
altura (R x []) = 1
altura (R x xs) = 1 + maximum (map altura xs)

prune :: Int -> RTree a -> RTree a
prune 1 (R x xs) = R x []
prune i (R x xs) = R x (map (prune (i-1)) xs)  

mirror :: RTree a -> RTree a 
mirror (R x xs) = R x (reverse (map mirror xs))

postorder :: RTree a -> [a]
postorder (R x []) = [x] 
postorder (R x xs) = [x] ++ concat (map postorder xs)

-- * Exercício 3
data LTree a = Tip a 
               | Fork (LTree a) (LTree a) deriving Show

ltree :: LTree Int
ltree = Fork (Tip 2) (Tip 5)

ltSum :: Num a => LTree a -> a 
ltSum (Tip a) = a
ltSum (Fork a b) = ltSum a + ltSum b

listaLT :: Num a => LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork a b) = listaLT a ++ listaLT b

ltHeight :: LTree a -> Int
ltHeight (Tip a) = 1
ltHeight (Fork a b) = 1 + max2 (ltHeight a) (ltHeight b)

-- * Exercício 4
data FTree a b = Leaf b | No a (FTree a b) (FTree a b) deriving Show

ftree1 = No 8 (No 1 (Leaf 5)
                    (No 2 (Leaf 6)
                          (Leaf 4)))
              (No 9 (No 10 (Leaf 3)
                           (Leaf 7))
                    (Leaf 5))

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf b) = (Empty, Tip b)
splitFTree (No a l r) = (Node a tt tts,Fork tu tus)
    where (tt,tu) = splitFTree l
          (tts,tus) = splitFTree r

--joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
