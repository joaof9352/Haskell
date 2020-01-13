data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

data Hora = H Int Int deriving (Show)
type Etapa = (Hora,Hora)
type Viagem = [Etapa]

--1 a
testE :: Etapa -> Bool
testE ((H a b), (H c d)) | a < c = True
                         | a > c = False
                         | otherwise = b < d
--1 b
testV :: Viagem -> Bool
testV (((H a b), (H c d)):[]) = testE ((H a b), (H c d))
testV (((H a b), (H c d)):((H e f), (H g h)):t) | testE ((H a b), (H c d)) = testE ((H e f), (H g h))
                                                | otherwise = False

--1 c 
horapc :: Viagem -> Etapa
horapc [((H e f), (H g h))] = ((H e f), (H g h))
horapc (h:t) = (fst h, snd (last t))

--1 d (Erro)
viageme :: Viagem -> Hora
viageme [] = H 0 0
viageme (((H e f), (H g h)):[]) = (H (g-e) (f-h))
viageme (((H a b), (H c d)):t) = normalizarHora (H ((c-a)+a2) ((d-b)+b2)) 
        where (H a2 b2) = viageme t

normalizarHora :: Hora -> Hora
normalizarHora (H a b) | b >= 60 = normalizarHora (H (a+1) (b-60))
                       | b < 0 = normalizarHora (H (a-1) (b+60))
                       | otherwise = (H a b)


--1 e
viagemte :: Viagem -> Hora
viagemte [] = H 0 0
viagemte (((H a b),(H c d)):[]) = H 0 0
viagemte (((H a b), (H c d)):((H e f), (H g h)):t) = normalizarHora (H (e-c+px) (f-d+py)) 
    where (H px py) = viagemte t  

type Poligonal = [Ponto]

radianos2graus :: Double -> Double
radianos2graus r = r * (180/pi)

graus2rad :: Double -> Double
graus2rad g = g * (pi/180)

polar2cart :: Ponto -> Ponto
polar2cart c@(Cartesiano x1 y1) = c
polar2cart (Polar r a) = Cartesiano (r * (cos $ graus2rad a)) (r * ( sin $ graus2rad a))

--2 a
compriPol :: Poligonal -> Double
compriPol [] = 0
compriPol ((Cartesiano c d):[]) = 0
compriPol (p:[]) = compriPol ((polar2cart p):[])
compriPol ((Cartesiano a b):(Cartesiano c d):ts) = dis + compriPol ((Cartesiano c d):ts)
    where dis = sqrt x
          x = (a-c)^2 + (b-d)^2
compriPol (h:t:ts) = compriPol ((polar2cart h):(polar2cart t):ts)

--2 b (ver de novo)
fechaPol :: Poligonal -> Bool
fechaPol [] = True
fechaPol ((Cartesiano a b):(Cartesiano c d):ts) | length ((Cartesiano a b):(Cartesiano c d):ts) > 2 = fechaPol ((Cartesiano a b):ts)
                                  | otherwise = (Cartesiano a b) == (Cartesiano c d)
fechaPol (p1:p2:ts) | length (p1:p2:ts) < 2 = fechaPol (p1:ts)
                    | otherwise = p1 == p2

--2 c 

--2 d


--3
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String 
              deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

--a 
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email [] = [(nome,[Email email])]
acrescEmail nome email ((name,h):t) | nome == name = (name,h ++ [Email email]):t
                                    | otherwise = acrescEmail nome email t

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails nome ((name,l):t) | nome == name = Just (checkEmail [(name,l)])
                            | otherwise = verEmails nome t

checkEmail :: Agenda -> [String]
checkEmail [(n,[])] = []
checkEmail [(n,((Email a):t))] = a:checkEmail [(n,t)]
checkEmail [(n,_:t)] = checkEmail [(n,t)]

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs ((Casa a):t) = a:consTelefs t
consTelefs ((Trab a):t) = a:consTelefs t
consTelefs ((Tlm a):t) = a:consTelefs t
consTelefs (_:t) = consTelefs t

casa :: Nome -> Agenda -> Maybe Integer
casa _ [] = Nothing
casa nome ((name,cons):t) | nome == name = retornaTlfCasa cons
                          | otherwise = casa nome t

retornaTlfCasa :: [Contacto] -> Maybe Integer
retornaTlfCasa [] = Nothing
retornaTlfCasa ((Casa a):t) = Just a
retornaTlfCasa (_:t) = retornaTlfCasa t

-- * Exercício 4
type Dia = Int
type Mes = Int
type Ano = Int

data Data = D Dia Mes Ano deriving Show

type TabDN = [(Nome,Data)]

procura :: Nome -> TabDN -> Maybe Data
procura name [] = Nothing
procura name ((nome,d):t) | nome == name = Just d
                          | otherwise = procura name t

idade :: Data -> Data -> Int
idade (D d1 m1 a1) (D d2 m2 a2) | m2 > m1 = a1-a2-1
                                | m2 == m1 && d2 > d1 = (a1-a2-1)
                                | otherwise = a1-a2 
                                

anterior :: Data -> Data -> Bool 
anterior (D d1 m1 a1) (D d2 m2 a2) | a1 < a2 = True
                                   | a1 == a2 && m1 < m2 = True
                                   | a1 == a2 && m1 == m2 && d1 < d2 = True
                                   | otherwise = False

ordena :: TabDN -> TabDN
ordena [] = []
ordena (h:t) = insertDN h (ordena t)

insertDN :: (Nome,Data) -> TabDN -> TabDN
insertDN a [] = [a]
insertDN (n1,d1) ((n2,d2):t) | anterior d1 d2 = (n1,d1):(n2,d2):t
                             | otherwise = (n2,d2):insertDN (n1,d1) t

porIdade :: Data -> TabDN -> [(Nome,Int)]
porIdade _ [] = []
porIdade d tabdn = porIdade d t ++ [(n,(idade d dn))]
    where ((n,dn):t) = ordena tabdn

data Movimento = Credito Float | Debito Float deriving Show

data Extracto = Ext Float [(Data, String, Movimento)] deriving Show

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext b ((_,_,Credito a):t)) f | a > f = (Credito a):extValor (Ext b t) f
extValor (Ext b ((_,_,Debito a):t)) f | a > f = (Debito a):extValor (Ext b t) f 
extValor (Ext b ((_,_,_):t)) f = extValor (Ext b t) f
extValor _ _ = []

filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext a ((dt,str,mov):t)) l | elem str l = (dt,mov):filtro (Ext a t) l
                                  | otherwise = filtro (Ext a t) l

creDeb :: Extracto -> (Float,Float)
creDeb (Ext _ []) = (0,0)
creDeb (Ext a ((_,_,mov):t)) | isCredito mov = (getValor mov + as, bs)
                             | otherwise = (as, getValor mov + bs)
    where (as,bs) = creDeb (Ext a t)


isCredito :: Movimento -> Bool
isCredito (Credito _) = True
isCredito _ = False

getValor :: Movimento -> Float
getValor (Credito a) = a
getValor (Debito a) = a

saldo :: Extracto -> Float
saldo (Ext a c) = a + cre - deb
   where (cre,deb) = creDeb (Ext a c)