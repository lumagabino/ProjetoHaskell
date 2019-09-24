--Luma Gabino Vasconcelos RA: 202495
--Pietro Ruy Pugliesi RA: 185921

import Data.List
import Data.Char
import Data.String

data Point = Point String [Float] deriving (Eq,Show,Read)
getName (Point name _) = name
getCoordinates (Point _ coord) = coord

data Label = Label String Int deriving (Eq,Show,Read)
getLabelName (Label name _) = name
getLabel (Label _ label) = label

data MinDistance = MinDistance String Point deriving (Eq,Show,Read)


main = do
    input <- getContents
    let linhas = lines input
    let separado = separaLinhaVazia linhas
    let first = fst separado
    let second = snd separado
   
    print "FIRST:"
    print first

    print "SECOND:"
    print second

    let pointList = createPointList first
    print "pointList:"
    print pointList

    let labelList = createLabelList second
    print "labelList:"
    print labelList

    let tupleList = associateLabelAndPoint labelList pointList
    print "tupleList:"
    print tupleList

    let pointsWithoutLabel = getPointsWithoutLabel labelList pointList
    print "pointsWithoutLabelList:"
    print pointsWithoutLabel

    -- calcula distancia entre ptos dos 2 grupos
    -- let distancias = (distance) <$> (map getCoordinates pointsWithoutLabel)  <*> (map getCoordinates  (snd tupleList))
    -- print distancias
   
    -- -- seleciona ponto sem label mais proximo de algum grupo com label
    -- let point = menor distancias
    -- --preciso saber para qual ponto é a menor distância!!!!!

    -- -- atribui esse label e atualiza os 2 grupos
    -- point = setPointLabel labelMenorDistancia Point
    -- -------------mas nao posso atribuir :(...

    -- repetir



    --printar resposta--> (LABEL, [pontosComEsseLabel])
    -- printResposta listaFinal


-- Separa pontos que ainda não possum label
getPointsWithoutLabel _ [] = []
getPointsWithoutLabel labelList (x:xs) = if (checkLabelList labelList x) == False
    then x:(getPointsWithoutLabel labelList xs)
    else getPointsWithoutLabel labelList xs


checkLabelList [] _ = False
checkLabelList (x:xs) point = if (getName point) == (getLabelName x)
    then True
    else checkLabelList xs point 

-- Cria lista de tuplas (Int, Ponto) -> (label, ponto) para todos os pontos que possuem label
associateLabelAndPoint :: [Label] -> [Point] -> [(Int, Point)]
associateLabelAndPoint [] _ = []
associateLabelAndPoint (x:xs) pointList = let
    point = getPointLabel x pointList
    tuple = ( (getLabel x) , point )
    tupleList = tuple:(associateLabelAndPoint xs pointList)
    in tupleList

getPointLabel :: Label -> [Point] -> Point
getPointLabel label (x:xs) = if (getName x) == (getLabelName label)
    then x
    else getPointLabel label xs

-- Transformação da segunda entrada na mesma estrutura de dados para facilitar as comparações
createLabelList [[]] = []
createLabelList (x:xs) = let
    v1 = words x
    name = getNameInput v1
    label = getLabelInput v1
    point = Label name label
    v2 = point:(createLabelList xs)
    in v2

-- Função para converter primeira parte da entrada em uma lista de Pontos
createPointList [[]] = []
createPointList (x:xs) = let
    v1 = words x
    name = getNameInput v1
    coord = getCoordinatesInput v1
    point = Point name coord
    v2 = point:(createPointList xs)
    in v2

getNameInput (x:xs) = x
getCoordinatesInput (x:xs) =  map (read :: String -> Float) xs
getLabelInput (x:xs)
    | xs == [] = read x :: Int
    | otherwise = getLabelInput xs

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

menor lista  = foldl1 (\acc x -> if acc < x then acc else x) lista


-- Distancia euclidiana
distance as bs = sqrt (dist as bs 0)

dist [] _ acc = acc
dist _ [] acc = acc
dist (p:ps) (q:qs) acc = dist ps qs (acc+((q - p) ^ 2)) 


-- printResposta listaFinalTuplas = 
--     let labelOrdenado = sort listaFinalTuplas -- label ordenado (1, []), (2, []), etc
--     in sort $ snd labelOrdenado