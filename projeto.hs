--Luma bla bla bla bla RA:
--Pietro Ruy Pugliesi RA: 185921

-- module Main where
--
-- main :: IO ()

data Point = Point { name :: String
                     , coordinates :: [Int]
                     , label :: Int
                     } deriving Show

main = do
    input <- getContents
    let linhas = lines input
    let separado = separaLinhaVazia linhas
    let first = fst separado
    let second =  snd separado

    print first
    print "--"
    print second

separaLinhaVazia lista = splitAt pos lista where pos = posicao "" lista

posicao _ [] = 0
posicao x (a:as) = if a == x
    then 1
    else 1 + posicao x as

    -- Point 1
    -- name1 <- getLine
    --
    -- inputX1 <- getLine
    -- let x1 = read inputX1 :: Int
    --
    -- inputY1 <- getLine
    -- let y1 = read inputY1 :: Int
    --
    -- inputZ1 <- getLine
    -- let z1 = read inputZ1 :: Int
    --
    -- let list1 = [x1, y1, z1]
    --
    -- -- let point1 = Point { name = name1 , coordinates = list1 }
    --
    -- -- Point 2
    -- name2 <- getLine
    --
    -- inputX2 <- getLine
    -- let x2 = read inputX2 :: Int
    --
    -- inputY2 <- getLine
    -- let y2 = read inputY2 :: Int
    --
    -- inputZ2 <- getLine
    -- let z2 = read inputZ2 :: Int
    --
    -- let list2 = [x2, y2, z2]
    --
    -- -- let point2 = Point { name = name2 , coordinates = list2 }
    --
    -- let d = distance list1 list2
    --
    -- print d


-- Distancia euclidiana
distance as bs = sqrt (dist as bs 0)

dist [] _ acc = acc
dist _ [] acc = acc
dist (p:ps) (q:qs) acc = dist ps qs (acc+((q - p) ^ 2))
