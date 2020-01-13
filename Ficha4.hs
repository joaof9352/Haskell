import Data.List

isDigit :: Char -> Bool
isDigit a | elem a ['0'..'9'] = True
          | otherwise = False

isAlpha :: Char -> Bool
isAlpha a | elem a ['a'..'z'] = True
          | elem a ['A'..'Z'] = True
          | otherwise = False

--Fazer ex 1 e 2 em casa

doisA = [2^x | x <- [0..10]]
doisB = [(x,y) | x <- [1..5], y <- [1..5], x + y == 6]
doisC = [ [1..x] | x <- [1..5]]
doisD = [ replicate x 1 | x <- [1..5]]
doisE = [ factorial x | x <- [1..6]]
    where factorial 0 = 1
          factorial x = x * factorial (x - 1)


--Ex3
digitAlpha3 :: String -> (String,String)
digitAlpha3 s = digitAlpha3Acc ([],[]) s

--Acumulador
digitAlpha3Acc :: (String,String) -> String -> (String,String)
digitAlpha3Acc (ds,as) [] = (ds,as)
digitAlpha3Acc (ds,as) (c:cs) | isDigit c = digitAlpha3Acc ((ds++[c]),as) cs
                              | isAlpha c = digitAlpha3Acc (ds,(as++[c])) cs
                              | otherwise = digitAlpha3Acc (ds,as) cs

--Ex 4
nzp :: [Int] -> (Int,Int,Int)
nzp a = nzpAcc (0,0,0) a

nzpAcc :: (Int,Int,Int) -> [Int] -> (Int,Int,Int)
nzpAcc (a,b,c) [] = (a,b,c)
nzpAcc (a,b,c) (h:t) | h < 0 = nzpAcc ((a+1),b,c) t
                     | h == 0 = nzpAcc (a,(b+1),c) t
                     | otherwise = nzpAcc (a,b,(c+1)) t

--Ex 5
mydivMod :: Integral a => a -> a -> (a,a)
mydivMod a b = mydivModAcc (0,0) a b 

mydivModAcc :: Integral a => (a,a) -> a -> a -> (a,a)
mydivModAcc (a,b) c d | c > d = mydivModAcc (a+1,b) (c-d) d
                      | c == d = (a+1,b) 
                      | otherwise = (a,c)
--Ex 6 
fromDigits :: [Int] -> Int
fromDigits l = fromDigitsAcc 0 (length l-1) l


fromDigitsAcc :: Int -> Int -> [Int] -> Int
fromDigitsAcc a x [] = a
fromDigitsAcc a x (h:t) | x >= 0 = fromDigitsAcc (a + (h * (10^x))) (x-1) t 
                        | otherwise = a

--Ex 7 
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maximum [sum m | m <- inits l]

--Ex8
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)