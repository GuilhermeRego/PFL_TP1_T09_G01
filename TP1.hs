import qualified Data.List 
import qualified Data.Array
import qualified Data.Bits

--4.1
type City = String
type Distance = Int
type Path = [City]
type RoadMap = [(City,City,Distance)]

--4.2
--1: Criar uma lista com compreensão de listas com todas as cidades que aparecem primeiro no roadmap e concatenar com
-- outra lista com todas as cidades que aparecem depois da primeira, e depois usar a função nub para
-- remover duplicados.
cities :: RoadMap -> [City]
cities roadMap = Data.List.nub ([c1 | (c1, c2, d) <- roadMap] ++ [c2 | (c1, c2, d) <- roadMap])

--2: Função recursiva, que verifica a cada "iteração" da recursão se as duas cidades que estamos a verificar
-- estão no trio atual.
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] _ _ = False
areAdjacent ((a,b,_):xs) c1 c2
    | (a == c1 && b == c2) || (a == c2 && b == c1) = True
    | otherwise = areAdjacent xs c1 c2

--3: À semelhança da última função, mas se encontrar uma adjacência entre as cidades, retorna a sua distância.
distance :: RoadMap -> City -> City -> Maybe Distance
distance ((a,b,d):xs) c1 c2
    | (a == c1 && b == c2) || (a == c2 && b == c1) = Just d
    | otherwise = distance xs c1 c2

--4: Função recursiva, que verifica se encontrámos um trio com a cidade que estamos à procura (passada
-- no segundo argumento), e caso estiver, inicia uma lista com a tupla constituída da cidade adjacente e
-- da distância entre as duas. Usamos a função nub para remover duplicados.
adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent [] _ = []
adjacent ((a,b,d):xs) c
    | (a == c) = Data.List.nub ((b, d) : (adjacent xs c))
    | (b == c) = Data.List.nub ((a, d) : (adjacent xs c))
    | otherwise = adjacent xs c

--5:
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance [] _ = Just 0
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance rm [x,y] = distance rm x y
pathDistance rm (x:y:xs)
    | areAdjacent rm x y 
    = case distance rm x y of
        Nothing -> Nothing
        Just d -> fmap (d +) (pathDistance rm (y:xs))
    | otherwise = Nothing





roadmap :: RoadMap
roadmap = [("New York", "Los Angeles", 2445),
               ("Chicago", "New York", 790),
               ("Houston", "Los Angeles", 1374),
               ("Chicago", "Houston", 1082)]
