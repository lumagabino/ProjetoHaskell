--Luma Gabino Vasconcelos RA: 202495
--Pietro Ruy Pugliesi RA: 185921

import Data.List
import Data.Char

data Point = Point { name :: String
                     , coordinates :: [Int]
                     , label :: Int
                     } deriving Show

main = do
    input <- getContents
    let linhas = lines input
    let separado = separaLinhaVazia linhas
    let first = fst separado
    let second = snd separado

    print first
    print "--"
    print second

    let pointList = createDataStruct first
    print pointList

    let labelList = createLabelList second
    print labelList


    -- let pointWithLabelList = setLabels pointList labelList
    -- print pointWithLabelList


-- setLabels (x:xs) labelList = let
--     v1 = getPointLabel labelList x
--     -- v2 = x . label =  v1
--     -- v3 = x:(setLabels xs labelList)
--     in v1

-- getPointLabel [] _ = -1
-- getPointLabel (x:xs) elem = if elem . name == x . name
--     then x . label
--     else getPointLabel xs elem

-- Transformação da segunda entrada na mesma estrutura de dados para facilitar as comparações
createLabelList [[]] = []
createLabelList (x:xs) = let
    v1 = words x
    v2 = getName v1
    v3 = getLabel v1
    v4 = Point { name = v2 , coordinates = [], label = v3 }
    v5 = v4:(createLabelList xs)
    in v5

-- Função para converter primeira parte da entrada em uma lista de Pontos
createDataStruct [[]] = []
createDataStruct (x:xs) = let
    v1 = words x
    v2 = getName v1
    v3 = getCoordinates v1
    v4 = Point { name = v2 , coordinates = v3, label = -1 }
    v5 = v4:(createDataStruct xs)
    in v5

getName (x:xs) = x
getCoordinates (x:xs) =  map (read :: String -> Int) xs
getLabel (x:xs) = let 
    v1 = map (read :: String -> Int) xs
    v2 = head v1
    in v2

-- Separa a entrada em
-- Primeira parte: pontos e suas coordenadas
-- Segunda parte: nomes dos pontos e seus respectivos labels
separaLinhaVazia lista = splitAt pos lista where pos = posicao "" lista

--
-- associaLabels:: [Point] -> [a] -> [Point]
-- associaLabels listaPontos [] = listaPontos
-- associaLabels (x:xs) (y:ys) = if getName x == getName y then

posicao _ [] = 0
posicao x (a:as) = if a == x
    then 1
    else 1 + posicao x as


-- Distancia euclidiana
distance as bs = sqrt (dist as bs 0)

dist [] _ acc = acc
dist _ [] acc = acc
dist (p:ps) (q:qs) acc = dist ps qs (acc+((q - p) ^ 2))
