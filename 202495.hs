--Luma Gabino Vasconcelos RA: 202495
--Pietro Ruy Pugliesi RA: 185921

import Data.List
import Data.Char
import Data.String

-- ponto no formato nome<String>, coordenadas <[Float]>, sem label<Int> ainda
data Point = Point String [Float] deriving (Eq,Show,Read)
--gets
getName (Point name _) = name
getCoordinates (Point _ coord) = coord

-- label no formato nome<String>, Label<Int> 
data Label = Label String Int deriving (Eq,Show,Read)
--gets
getLabelName (Label name _) = name
getLabel (Label _ label) = label

main = do
    input <- getContents--le entrada
    let linhas = lines input--separa em linhas
    let separado = separaLinhaVazia linhas--separa a linha vazia entre pontos e labels
    let first = fst separado--pontos
    let second = snd separado--labels

    let pointList = createPointList first

    let labelList = createLabelList second

    let tupleList = associateLabelAndPoint labelList pointList--lista de tuplas
    --no formato(label, Point)

    --lista de pontos sem label para calcular distancias
    let pointsWithoutLabel = getPointsWithoutLabel labelList pointList

    --lista final de pontos ja com labels associados
    -- no formato [(Label, Point), ...]
    let final = repeatUntilEmpty pointsWithoutLabel tupleList

    --separa os pontos com mesmo label
    let uniqueSorted = nub (sort (map fst final))

    --saida final
    let out = outputFomatter final uniqueSorted
    mapM_ print out    


-------------------------------

-- Separa a entrada em
-- Primeira parte: pontos e suas coordenadas
-- Segunda parte: nomes dos pontos e seus respectivos labels
separaLinhaVazia lista = splitAt pos lista where pos = posicao "" lista

posicao _ [] = 0
posicao x (a:as) = if a == x
    then 1
    else 1 + posicao x as

-- Converter primeira parte da entrada em uma lista de Pontos
createPointList [[]] = []
createPointList (x:xs) = let
    v1 = words x
    name = getNameInput v1
    coord = getCoordinatesInput v1
    point = Point name coord
    v2 = point:(createPointList xs)
    in v2

getNameInput [] = []
getNameInput (x:_) = x

getCoordinatesInput (x:xs) =  map (read :: String -> Float) xs

getLabelInput (x:xs)
    | xs == [] = read x :: Int
    | otherwise = getLabelInput xs

-- Transformação da segunda entrada na mesma estrutura de dados
-- para facilitar as comparações
createLabelList [] = []
createLabelList (x:xs) = let
    v1 = words x
    name = getNameInput v1
    label = getLabelInput v1
    point = Label name label
    v2 = point:(createLabelList xs)
    in v2

-- Cria lista de tuplas (Int, Point) = (Label, Point)
-- para todos os pontos que possuem label
associateLabelAndPoint :: [Label] -> [Point] -> [(Int, Point)]
associateLabelAndPoint [] _ = []
associateLabelAndPoint (x:xs) pointList = let
    point = getPointLabel x pointList
    tuple = ( (getLabel x) , point )
    tupleList = tuple:(associateLabelAndPoint xs pointList)
    in tupleList

--retorna ponto com o label dado como parametro
getPointLabel :: Label -> [Point] -> Point
getPointLabel label (x:xs) = if (getName x) == (getLabelName label)
    then x
    else getPointLabel label xs


-- Separa pontos que ainda não possum label
getPointsWithoutLabel _ [] = []
getPointsWithoutLabel labelList (x:xs) = if (checkLabelList labelList x) == False
    --se ponto nao tem label, adiciona na lista
    then x:(getPointsWithoutLabel labelList xs)
    else getPointsWithoutLabel labelList xs

--checa se o ponto está na lista de pontos com label
--ou seja, se deve conter um label
checkLabelList [] _ = False
checkLabelList (x:xs) point = if (getName point) == (getLabelName x)
    then True
    else checkLabelList xs point 
    
--calcula distancias e associa label ate 
--lista de pontos sem label vazia
repeatUntilEmpty [] list = list
repeatUntilEmpty noLabelList labledList = let
    allDistances = getDistances noLabelList labledList
    dist = minDist allDistances--distancia minima
    tupleToAdd = (getNameTuple dist, getPointTuple dist)
    listAdded = labledList++[tupleToAdd]--adiciono ponto na lista com label
    listAfterRemove = remove noLabelList (getPointTuple dist)--remove da lista sem label
    repeated = repeatUntilEmpty listAfterRemove listAdded --recursao ate lista sem label vazia
    in repeated

--menor da lista de distancias
minDist :: [(Int, Float, Point)] -> (Int, Float, Point)
minDist (x:xs) = min' xs x

--menor da lista, comparacao das tuplas
--a partir da distancia (Float), segundo item da tupla
min' :: [(Int, Float, Point)] -> (Int, Float, Point) -> (Int, Float, Point)
min' [] a = a
min' (y:ys) a
    | (getDistTuple y) < (getDistTuple a) = min' ys y
    | otherwise = min' ys a

--operacoes na tupla
getNameTuple (name,_,_) = name
getDistTuple (_,dist,_) = dist
getPointTuple (_,_,point) = point

--calcula diustancia entre um ponto sem label e uma lista de pontos com label
getDistances _ [] = []
getDistances pointsWithoutLabel (x:xs) = let 
    tupleList = getListOfDistances pointsWithoutLabel x
    distList = tupleList++(getDistances pointsWithoutLabel xs)
    in distList

--chamo getDistances para todos os pontos sem label
--assim calculo todas as distancias
getListOfDistances [] _ = []
getListOfDistances (x:xs) tuple = let
    coord = getCoordinates (snd tuple)
    dist = distance coord (getCoordinates x)
    label = fst tuple
    tupleDistance = (label, dist, x)
    list = tupleDistance:(getListOfDistances xs tuple)
    in list

-- Distancia euclidiana
distance as bs = sqrt (dist as bs 0)

dist [] _ acc = acc
dist _ [] acc = acc
dist (p:ps) (q:qs) acc = dist ps qs (acc+((q - p) ^ 2)) 

--remove item da lista, para remover da lista dos pontos sem label
remove [] _ = []
remove (x:xs) point = if (getName point) == (getName x)
    then remove xs point
    else x : remove xs point

--formata a lista de tuplas para a saida
outputFomatter :: [(Int, Point)] -> [Int] -> [(Int, [String])]
outputFomatter _ [] = []
outputFomatter final (x:xs) = let
    pointNames = searchForPointNames final x
    sortedPoints = sort pointNames
    format = (x, sortedPoints)
    list = format:(outputFomatter final xs)
    in list

--lista de nomes dos pontos que possuem um dado label
searchForPointNames [] _ = []
searchForPointNames (x:xs) label = if (fst x) == label
    then (getName (snd x)):(searchForPointNames xs label)
    else searchForPointNames xs label