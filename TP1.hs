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
distance [] _ _ = Nothing
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

--5: Função recursiva que utiliza a função distance para verificar se existe
-- um path entre as duas cidades, e se sim, retornar a sua distância, e
-- somar a esse valor a distância entre as duas próximas cidades.
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance [] _ = Just 0
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance rm [x,y] = distance rm x y
pathDistance rm (x:y:xs)
    = case distance rm x y of
        Nothing -> Nothing
        Just d -> fmap (d +) (pathDistance rm (y:xs))

--6: Dividimos o problema em sub-problemas, e que cada função resolve um sub-problema. Começando na função mais interior (cities) para
-- a função mais exterior (highest), primeiro começámos por listar todas as cidades do roadmap (cities rm) e passar como argumento na
-- função cityAdjacent para esta criar uma lista de tuplos (City, Int), com o primeiro e segundo elemento sendo, respetivamente, a
-- cidade e quantas adjacências tem, essa função usa uma função definida anteriormente adjacent. Posteriormente, aplicamos a função
-- sortAdjacent que ordena de forma decrescente e pelo segundo elemento da tupla (da cidade com mais adjacências, para a que tem menos)
-- e, finalmente, aplicamos a função highest que adiciona a uma lista as cidades com mais adjancências, retornando assim a lista de
-- cidades com mais adjacências.
rome :: RoadMap -> [City]
rome rm = highest (sortAdjacent (cityAdjacent rm (cities rm)))

cityAdjacent :: RoadMap -> [City] -> [(City, Int)]
cityAdjacent _ [] = []
cityAdjacent rm (x:xs) = (x, length (adjacent rm x)) : (cityAdjacent rm xs)

sortAdjacent :: [(City, Int)] -> [(City, Int)]
sortAdjacent [] = []
sortAdjacent [xs] = [xs]
sortAdjacent ((c1, n1):(c2, n2):xs)
    | n1 > n2 = [(c1, n1)] ++ [(c2, n2)] ++ sortAdjacent xs
    | n1 <= n2 = [(c2, n2)] ++ [(c1, n1)] ++ sortAdjacent xs

highest :: [(City, Int)] -> [City]
highest [] = []
highest [(c, n)] = [c]
highest ((c1, n1):(c2,n2):xs)
    | n1 == n2 = [c1] ++ [c2] ++ highest xs
    | otherwise = [c1]

-- 7: A função principal que verifica se um roadmap (grafo não direcionado) é fortemente conectado.
-- Para isso, dividimos o problema em sub-problemas e utilizamos uma abordagem de busca em profundidade (DFS).
-- A lógica é a seguinte:
-- 1. Primeiro, obtemos a lista de todas as cidades usando a função cities.
-- 2. Depois, realizamos uma busca em profundidade a partir da primeira cidade da lista usando a função dfs.
-- 3. Se a quantidade de cidades visitadas durante a busca for igual ao total de cidades no roadmap , quer dizer que todas 
-- já foram visitadas, então o grafo é fortemente conectado, retornando True. Caso contrário, retorna False.
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadMap =
    let allCities = cities roadMap  
        visitedCities = dfs roadMap (head allCities) []  
    in length visitedCities == length allCities  


-- Realiza uma busca em profundidade (DFS) nas cidades do roadmap.
-- A função recebe um RoadMap, uma City e uma lista de cidades visitadas (visited).
-- Se a cidade já foi visitada, retorna a lista de cidades visitadas.
-- Caso contrário, adiciona a cidade à lista de visitados e continua à procura de cidades adjacentes.
dfs :: RoadMap -> City -> [City] -> [City]
dfs roadMap city visited
    | elem city visited = visited  
    | otherwise = foldl (\acc neighbor -> dfs roadMap neighbor acc) (city:visited) (adjacentCities roadMap city)

-- Encontra cidades adjacentes a uma cidade no RoadMap.
-- A função verifica o RoadMap e retorna uma lista de cidades adjacentes a partir da cidade dada.
-- Para isso, utiliza uma compreensão de listas que extrai as cidades adjacentes tanto na posição de origem quanto na de destino.
adjacentCities :: RoadMap -> City -> [City]
adjacentCities roadMap city = [b | (a, b, _) <- roadMap, a == city] ++ [a | (a, b, _) <- roadMap, b == city]








rm :: RoadMap
rm = [("New York", "Los Angeles", 2445),
               ("Chicago", "New York", 790),
               ("Houston", "Los Angeles", 1374),
               ("Chicago", "Houston", 1082),
               ("New York", "Las Vegas", 2068)]

-- Exemplo de RoadMaps para testar a função isStronglyConnected
-- Exemplo 1: Grafo com arestas duplicadas
rm1 :: RoadMap
rm1 = [("A", "B", 10),
        ("B", "A", 10), -- Aresta duplicada
        ("B", "C", 20)]
-- Resultado esperado: True
-- Justificação: "A" pode alcançar "B" e "B" pode alcançar "C", então o grafo é fortemente conexo.

-- Exemplo 2: Grafo isolado
rm2 :: RoadMap
rm2 = [("A", "B", 10),
        ("C", "D", 20)]
-- Resultado esperado: False
-- Justificação: "A" e "B" estão em um componente, enquanto "C" e "D" estão em outro.

-- Exemplo 3: Ciclo em apenas uma direção
rm3 :: RoadMap
rm3 = [("A", "B", 10),
        ("B", "C", 20),
        ("C", "A", 30),
        ("D", "E", 40)]
-- Resultado esperado: False
-- Justificação: O ciclo "A", "B", "C" não conecta com "D" e "E".

-- Exemplo 4: Grafo com várias conexões
rm4 :: RoadMap
rm4 = [("A", "B", 10),
        ("A", "C", 15),
        ("C", "B", 5),
        ("D", "E", 10)]
-- Resultado esperado: False
-- Justificação: "A", "B", e "C" estão em um componente, enquanto "D" e "E" estão em outro.

-- Exemplo 5: Grafo com uma cidade conectada a si mesma
rm5 :: RoadMap
rm5 = [("A", "A", 0)]
-- Resultado esperado: True
-- Justificação: "A" é a única cidade e está conectada a si mesma.

-- Exemplo 6: Grafo com cidade não conectada
rm6 :: RoadMap
rm6 = [("A", "B", 10),
        ("C", "D", 20)]
-- Resultado esperado: False
-- Justificação: "A" e "B" estão conectadas, mas "C" e "D" estão em um componente separado.

-- Exemplo 7: Cidade conectada a várias outras
rm7 :: RoadMap
rm7 = [("A", "B", 10),
        ("A", "C", 15),
        ("B", "C", 20),
        ("D", "E", 25)]
-- Resultado esperado: False
-- Justificação: "A", "B", e "C" estão em um componente, enquanto "D" e "E" estão em outro.
