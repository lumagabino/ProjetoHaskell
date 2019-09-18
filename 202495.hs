--Luma Gabino Vasconcelos RA: 202495
--Pietro Ruy Pugliesi RA: 185921

import Data.List
import Data.Char
import Data.String

data Point = Point String [Float] Int deriving (Eq,Show,Read)
getName (Point name _ _) = name
getCoordinates (Point _ coord _) = coord
getLabel (Point _ _ label) = label

main = do
    input <- getContents
    let linhas = lines input
    let separado = separaLinhaVazia linhas
    let first = fst separado
    let second = snd separado

    print first
    print "--"
    print second

    let pointList = populateData first
    print pointList

    let labelList = createLabelList second
    print labelList


--     let pointWithLabelList = setLabels pointList labelList
--     print pointWithLabelList


-- setLabels (x:xs) labelList = let
--     v1 = getPointLabel labelList x
--     -- v2 = x . label =  v1
--     -- v3 = x:(setLabels xs labelList)
--     in v1

-- getPointLabel [] _ = -1
-- getPointLabel (x:xs) point = if map (name . point) == map (name . x)
--     then (label x)
--     else getPointLabel xs elem

-- Transformação da segunda entrada na mesma estrutura de dados para facilitar as comparações
createLabelList [[]] = []
createLabelList (x:xs) = let
    v1 = words x
    name = getNameInput v1
    label = getLabelInput v1
    point = Point name _ label
    v2 = point:(createLabelList xs)
    in v2

-- Função para converter primeira parte da entrada em uma lista de Pontos
populateData [[]] = []
populateData (x:xs) = let
    v1 = words x
    name = getNameInput v1
    coord = getCoordinatesInput v1
    point = Point name coord _
    v2 = point:(populateData xs)
    in v2

getNameInput (x:xs) = x
getCoordinatesInput (x:xs) =  map (read :: String -> Int) xs
getLabelInput (x:xs) = let 
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

