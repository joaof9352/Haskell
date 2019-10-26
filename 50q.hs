import Data.List

--Ex1

enumFromTos :: Int -> Int -> [Int]
enumFromTos a b | a > b = []
                | a == b = [a]
                | otherwise = (a:(enumFromTos (a+1) b))

--Ex2
enumFromThenTos :: Int -> Int -> Int -> [Int]
enumFromThenTos a b c | a > c = []
                      | a == c = [a]
                      | otherwise = (a:(enumFromThenTos b (2*b-a) c))

--Ex3
juntar :: [a] -> [a] -> [a]
juntar [] l = l
juntar (h:t) l = (h:(juntar t l))

--Ex4
encontrar :: [a] -> Int -> a
encontrar (h:t) n | n == 0 = h
                  | otherwise = encontrar t (n-1)

--Ex5
reverter :: [a] -> [a]
reverter (h:[]) = [h]
reverter (h:t) = reverter t ++ [h]

--Ex6
tomar :: Int -> [a] -> [a]
tomar n (h:t) | n >= (length (h:t)) = (h:t)
              | n == 0 = []
              | otherwise = (h:(tomar (n-1) t))

--Ex7
tirar :: Int -> [a] -> [a]
tirar n (h:[]) = []
tirar n (h:t) | n == 0 = (h:t)
              | (length (h:t)) <= n = []
              | otherwise = tirar (n-1) t

--Ex8
zipar :: [a] -> [b] -> [(a,b)]
zipar (h:[]) (a:[]) = ((h,a):[])
zipar (h:t) (a:b) = ((h,a):(zipar t b)) 
zipar _ [] = []
zipar [] _ = []

--Ex9
elemento :: Eq a => a -> [a] -> Bool
elemento a (h:[]) = a == h
elemento a (h:t) = if a == h then True else elemento a t

--Ex10
replicar :: Int -> a -> [a]
replicar 0 a = []
replicar n a = (a:(replicar (n-1) a))

--Ex11
intersecer :: a -> [a] -> [a]
intersecer _ (h:[]) = [h]
intersecer a (h:t) = (h:a:(intersecer a t))

--Ex12
mygroup :: Eq a => [a] -> [[a]]
mygroup l@(h:ts) = (aux3 h l):(mygroup (aux4 h ts))
mygroup _ = []

aux3 :: Eq a => a -> [a] -> [a]
aux3 a (t:ts) | a == t = t:(aux3 a ts)
              | otherwise = []
aux3 _ [] = []

aux4 :: Eq a => a -> [a] -> [a]
aux4 x (y:ys) | x == y = aux4 x ys
              | otherwise = y:ys
aux4 _ [] = []

--OU

mygroup' :: Eq a => [a] -> [[a]]
mygroup' [] = []
mygroup' (h:t) = (takeWhile (==h) (h:t)):mygroup' (dropWhile (==h) (h:t))

--Ex13

concatar :: [[a]] -> [a]
concatar [] = []
concatar (x:xs) = x ++ concatar xs

--Ex14 
myinits :: [a] -> [[a]]
myinits [] = [[]] 
myinits (h:t) = myinits (init (h:t)) ++ [h:t]

--Ex15
mytails :: [a] -> [[a]]
mytails [] = [[]]
mytails (h:t) = (h:t):mytails t

--Ex16
prefixo :: Eq a => [a] -> [a] -> Bool
prefixo [] _ = True
prefixo (h:[]) (x:_) = h == x
prefixo _ [] = False
prefixo (h:hs) (x:xs) | x == h = prefixo hs xs
                      | otherwise = False

--Ex17
sufixo :: Eq a => [a] -> [a] -> Bool
sufixo [] _ = True
sufixo (h:[]) (x:[]) = h == x
sufixo _ [] = False
sufixo (h:hs) (x:xs) | length (h:hs) == length (x:xs) = if h == x then sufixo hs xs else False 
                     | length (h:hs) < length (x:xs) = sufixo (h:hs) xs
                     | otherwise = False 

--Ex18
sequen :: Eq a => [a] -> [a] -> Bool
sequen [] _ = True
sequen (h:[]) (x:[]) = h == x
sequen _ [] = False
sequen (h:hs) (x:xs) | length (h:hs) == length (x:xs) = if h == x then sequen hs xs else False
                     | length (h:hs) < length (x:xs) = if h == x then sequen hs xs else sequen (h:hs) xs
                     | otherwise = False

--Ex19
myElemIndices :: Eq a => a -> [a] -> [Int]
myElemIndices _ [] = []
myElemIndices a (h:t) = (aux3 0 a (h:t))

aux3 :: Eq a => Int -> a -> [a] -> [Int]
aux3 n a [] = []
aux3 n a (h:t) | a == h = n:aux3 (n+1) a t
               | otherwise = aux3 (n+1) a t

--Ex20
myNub :: Eq a => [a] -> [a]
myNub [] = []
myNub (h:t) = if elem h t then myNub t else h:myNub t

--Ex21
myDelete :: Eq a => a -> [a] -> [a]
myDelete _ [] = [] 
myDelete a (h:hs) | a == h = hs
                  | otherwise = h:myDelete a hs
 
--Ex22 
myRemove :: Eq a => [a] -> [a] -> [a]
myRemove a [] = a
myRemove l (y:ys) = myRemove (myDelete y l) ys

--Ex23 
myUnion :: Eq a => [a] -> [a] -> [a]
myUnion l [] = l
myUnion [] l = l
myUnion l (h:hs) | elem h l = myUnion l hs
                 | otherwise = myUnion (l++[h]) hs

--Ex24
myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect l [] = []
myIntersect [] l = []
myIntersect (t:ts) l | elem t l = t:(myIntersect (ts) l)
                     | otherwise = myIntersect ts l 
 
--Ex25
myInsert :: Ord a => a -> [a] -> [a]
myInsert a [] = [a]
myInsert a (h:hs) | a <= h = a:h:hs 
                  | otherwise = h:(myInsert a hs)

--Ex26
myUnWords :: [String] -> String
myUnWords [x] = x 
myUnWords (h:t) = h ++ " " ++ myUnWords t

--Ex27
myUnLines :: [String] -> String
myUnLines [x] = x ++ "\n"
myUnLines (h:t) = h ++ "\n" ++ myUnLines t 

--Ex28
pMaior :: Ord a => [a] -> Int
pMaior (h:t) = aux5 (h,0) 1 t 

aux5 :: Ord a => (a,Int) -> Int -> [a] -> Int
aux5 (m,pm) p [] = pm
aux5 (m,pm) p (h:t) | m < h = aux5 (h,p) (p+1) t
                    | otherwise = aux5 (m,pm) (p+1) t
             
--Ex29 
myTemRepetidos :: Eq a => [a] -> Bool
myTemRepetidos [] = False
myTemRepetidos (h:ts) = elem h ts || myTemRepetidos ts

--Ex30
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t) = if elem h ['0'..'9'] then h:algarismos t else algarismos t 

--Ex31
myPosImpares :: [a] -> [a]
myPosImpares [] = []
myPosImpares (h:[]) = []
myPosImpares (h:t:ts) = t:(myPosImpares ts)

--Ex32
myPosPares :: [a] -> [a]
myPosPares [] = []
myPosPares (h:[]) = [h]
myPosPares (h:t:ts) = h:myPosPares ts

--Ex33
myIsSorted :: Ord a => [a] -> Bool
myIsSorted [h] = True
myIsSorted (h:t:ts) | h < t = myIsSorted (t:ts)
                    | otherwise = False 
                      
--Ex34
myiSort :: Ord a => [a] -> [a]
myiSort [] = []
myiSort (h:ts) = insere h (myiSort ts)

insere :: Ord a => a -> [a] -> [a] 
insere a [] = [a]
insere a (h:t) | a >= h = h:a:t
               | otherwise = h:insere a t

--Ex35
menor :: String -> String -> Bool
menor _ [] = True
menor [] _ = False
menor (h:t) (x:xs) = h < x || menor t xs

--Ex36
myelemMSet :: Eq a => a -> [(a,Int)] -> Bool
myelemMSet a [(h,t)] = a == h
myelemMSet a ((h,t):xs) = a == h || myelemMSet a xs

--Ex37
mylengthMSet :: [(a,Int)] -> Int
mylengthMSet [] = 0
mylengthMSet [(a,t)] = t
mylengthMSet ((h,t):xs) = t + mylengthMSet xs

--Ex38
convertMSet :: [(a,Int)] -> [a]
convertMSet [] = []
convertMSet ((a,i):xs) | i > 0 = a:convertMSet ((a,(i-1)):xs)
                       | otherwise = convertMSet xs 

--Ex39
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = []
insereMSet a ((b,c):xs) | a == b = (b,(c+1)):xs
                        | otherwise = (b,c):(insereMSet a xs)

--Ex40
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [] = [] 
removeMSet a ((b,c):xs) | a == b = (b,(c-1)):xs
                        | otherwise = (b,c):(insereMSet a xs)

--Ex41
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = aux6 1 h t:constroiMSet (aux7 h t)

aux6 :: Ord a => Int -> a -> [a] -> (a,Int)
aux6 n a [] = (a,n)
aux6 n a (h:t) | a == h = aux6 (n+1) a t
               | otherwise = (a,n)

aux7 :: Ord a => a -> [a] -> [a]
aux7 a [] = []
aux7 a l@(h:t) | a == h = aux7 a t
               | otherwise = l


--Ex42
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers ((Left a):t) = (a:as,bs)
    where (as,bs) = partitionEithers t
partitionEithers ((Right b):t) = (as,b:bs)
    where (as,bs) = partitionEithers t 

--Ex43
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (h:t) = case h of Just h -> h:catMaybes t
                            Nothing -> catMaybes t

data Movimento = Norte | Sul | Este | Oeste deriving Show

--Ex44 
posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x,y) (h:hs) = posicao (case h of Norte -> (x,y+1)
                                          Sul -> (x,y-1)
                                          Este -> (x+1,y)
                                          Oeste -> (x-1,y)) hs

--Ex45
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1) (x2,y2) | y2 - y1 > 0 = Norte:(caminho (x1,y1) (x2,(y2-1)))
                        | y2 - y1 < 0 = Sul:(caminho (x1,y1) (x2,(y2+1)))
                        | x2 - x1 > 0 = Este:(caminho (x1,y1) ((x2-1),y2))
                        | x2 - x1 < 0 = Oeste:(caminho (x1,y1) ((x2+1),y2))
                        | otherwise = []

--Ex46
vertical :: [Movimento] -> Bool
vertical (h:[]) = if (case h of Norte -> True
                                Sul -> True
                                Este -> False
                                Oeste -> False) then True else False
vertical (h:hs) = if (case h of Norte -> True
                                Sul -> True
                                Este -> False
                                Oeste -> False) then vertical hs else False

data Posicao = Pos Int Int deriving Show

--Ex47
maiscentral :: [Posicao] -> Posicao
maiscentral (h:[]) = h
maiscentral ((Pos x1 y1):(Pos x2 y2):ts) = if dis1 < dis2 then maiscentral ((Pos x1 y1):ts) else maiscentral ((Pos x2 y2):ts)
  where dis1 = (x1)^2 + (y1)^2
        dis2 = (x2)^2 + (y2)^2
        
--48
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos (Pos x1 y1) [] = []
vizinhos (Pos x1 y1) ((Pos x2 y2):ts) | ((abs (x1-x2)) <= 1) && ((abs (y1-y2)) <= 1 ) = (Pos x2 y2):vizinhos (Pos x1 y1) ts
                                      | otherwise = vizinhos (Pos x1 y1) ts

--49
mesmaOrd :: [Posicao] -> Bool
mesmaOrd (Pos x1 y1:[]) = True
mesmaOrd (Pos x1 y1:Pos x2 y2:xs) = if y1 == y2 then mesmaOrd (Pos x1 y1:xs) else False


--Ex50
data Semaforo = Verde | Amarelo | Vermelho deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK l = length [x | x <- l, case x of Vermelho -> False; otherwise -> True ] < 2
