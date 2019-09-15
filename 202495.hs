--Luma Gabino Vasconcelos RA: 202495
--Pietro Ruy Pugliesi RA: 185921


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

    let labelList = map words second
    print labelList



-- Função para pegar os nomes dos pontos (primeira palavra de qualquer linha da entrada)
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
