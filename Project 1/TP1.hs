import qualified Data.List
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

------------------------------------------------------------------------------------------------------------------------------

type City = String
type Path = [City]
type Distance = Int
type RoadMap = [(City,City,Distance)]

------------------------------------------------------------------------------------------------------------------------------  

-- Extracts unique cities from a RoadMap, a list of (city1, city2, distance) tuples, representing roads between cities.
--
-- Parameters:
--   roadmap - A list of tuples, where each tuple represents a road with two cities and a distance.
--
-- Returns: A list of cities appearing in the roadmap, with duplicates removed by using Data.List.nub.

cities :: RoadMap -> [City]
cities roadmap = Data.List.nub [city | (city1, city2, _) <- roadmap, city <- [city1, city2]]

------------------------------------------------------------------------------------------------------------------------------

-- Returns a boolean indicating whether two cities are linked directly.
--
-- Parameters:
--   roadmap - A list of tuples, where each tuple represents a road with two cities and a distance.
--   city1 - First city of the two given.
--   city2 - Second city of the two given.
--
-- Returns: If city1 and city2 are linked by a road (city1, city2, distance) or (city2, city, distance).

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadmap city1 city2 = or [(src, dest) == (city1, city2) || (src, dest) == (city2, city1) | (src, dest, _) <- roadmap]

------------------------------------------------------------------------------------------------------------------------------

-- Returns the distance between two cities if connected directly, otherwise returns Nothing.
--
-- Parameters:
--   roadmap - A list of tuples, where each tuple represents a road with two cities and a distance.
--   city1 - First city of the two given.
--   city2 - Second city of the two given.
--
-- Returns: The distance between city1 and city2 by using Data.List.find alongside with a lambda function 
--          that finds roads of the type (city1, city2, d) or (city2, city1, d) and Nothing if a road does not exist.

distance :: RoadMap -> City -> City -> Maybe Distance
distance roadmap city1 city2 = 
        case Data.List.find (\(src, dest, _) -> (src == city1 && dest == city2) || (src == city2 && dest == city1)) roadmap of
        Nothing -> Nothing
        Just (_, _, dist) -> Just dist

------------------------------------------------------------------------------------------------------------------------------

-- Returns the cities adjacent to a particular city and the respective distances between them.
--
-- Parameters:
--   roadmap - A list of tuples, where each tuple represents a road with two cities and a distance.
--   city - A particular city given.
--
-- Returns: The concatenation of two lists, one with the roads (city, otherCity, distance) 
--          and another with the roads (otherCity, city, distance).

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadmap city = [(dest, dist)| (src, dest, dist) <- roadmap, city == src] ++ [(src, dist)| (src, dest, dist) <- roadmap, city == dest]

------------------------------------------------------------------------------------------------------------------------------

-- Returns the sum of all individual distances in a path between two cities, if the consecutive pairs of cities
-- are connected by roads, and Nothing otherwise.
--
-- Parameters:
--   roadmap - A list of tuples, where each tuple represents a road with two cities and a distance.
--   city - A list of cities representing the path to be traveled.
--
-- Returns: The sum of all road distances between the cities in the path using recursion 
--          and Nothing if any pair of consecutive cities in the path is not directly connected by a road.

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance roadmap (city1:city2:path) = 
    do dist <- distance roadmap city1 city2
       dist_rest <- pathDistance roadmap (city2:path)
       return (dist + dist_rest)

------------------------------------------------------------------------------------------------------------------------------

-- Helper Function that returns a list of unique cities along with their respective counts of direct connections (roads).
--
-- Parameters:
--   roadmap - A list of tuples, where each tuple represents a road connecting two cities and its distance.
--
-- Returns: A list of tuples, where each tuple consists of a city and the number of direct connections to it by creating a 
--          a list of pairs of cities connected by direct roads and another with the unique Cities using the cities function. 

romeAux :: RoadMap -> [(City, Int)]
romeAux roadmap = 
    let uniqueCities = cities roadmap
        allCities = [pair | (city1, city2, _) <- roadmap, pair <- [city1, city2]]
    in [(city, length [c | c <- allCities , c == city]) | city <- uniqueCities]

------------------------------------------------------------------------------------------------------------------------------

-- Returns the names of the cities with the highest number of roads connecting to them (vertices with the highest degree).
--
-- Parameters:
--   roadmap - A list of tuples, where each tuple represents a road connecting two cities and its distance.
--
-- Returns: A list of cities that have the maximum number of direct connections (roads) in the given roadmap.
--          If multiple cities have the same highest number of connections, all those cities are returned.

rome :: RoadMap -> [City]
rome roadmap = 
    let tupleList = romeAux roadmap
        maxVal = maximum [b | (a,b) <- tupleList]
    in [city | (city, n) <- tupleList , n == maxVal]

------------------------------------------------------------------------------------------------------------------------------

-- Helper Function that returns a list of cities and their respective directly connected neighboring cities.
--
-- Parameters:
--   roadmap - A list of tuples, where each tuple represents a road connecting two cities and its distance.
--
-- Returns: A list of tuples, where each tuple contains a city and a list of cities directly connected to it.
--          providing an adjacency list representation of the roadmap.  

getAdjacencyList :: RoadMap -> [(City, [City])]
getAdjacencyList roadmap = [(city1, [cities | (cities,_) <- adjacent roadmap city1 ]) | city1 <- cities roadmap]

------------------------------------------------------------------------------------------------------------------------------

-- Helper Function that performs a depth-first search (DFS) starting from a given city to explore all reachable cities.
--
-- Parameters:
--   city - The current city being explored in the DFS.
--   paths - A list of tuples representing the adjacency list of cities and their directly connected neighbors.
--   visited - A list of cities that have already been visited in the DFS.
--
-- Returns: A list of all cities visited during the DFS, including the current city and its neighbors by using a fold left
--          and acc that accumulates the visited cities as the DFS proceeds through each neighbor.

dfs :: City -> [(City, [City])] -> [City] -> [City]
dfs city paths visited 
    | city `elem` visited = visited
    | otherwise = foldl (\acc neighbor -> dfs neighbor paths acc) (city : visited) neighbors
        where
            neighbors = case Data.List.find (\(c,_) -> c == city) paths of
                Just (_, connectedCities) -> connectedCities
                Nothing -> []

------------------------------------------------------------------------------------------------------------------------------ 

-- Checks if a roadmap of cities is strongly connected, meaning there is a path between every pair of cities.
--
-- Parameters:
--   roadmap - A list of tuples representing the roads between cities and their distances.
--
-- Returns: True if the graph is strongly connected, False otherwise with the help of the dfs and getAdjacencyList functions.

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadmap = 
    let adjList = getAdjacencyList roadmap
        nCities = length adjList
        (city, _, _) = head roadmap
    in  length (dfs city adjList []) == nCities 

-- os grafos são undirected, por isso a única maneira de não serem strongly connected
-- é se existirem 2 componentes completamente separados
-- por isso um dfs por qualquer node deve ser suficiente para chegar a todos os nodes
-- quando testei no gTest4 foi de 1.6s para 0.6s

------------------------------------------------------------------------------------------------------------------------------

removeCity :: City -> RoadMap -> RoadMap
removeCity city = filter (\(city1, city2, _) -> city1 /= city && city2 /= city)

-- Recursively find all paths between two cities
allPaths :: RoadMap -> City -> City -> [(Path, Distance)]
allPaths roadmap city1 city2 = go roadmap city1 city2 [] 0
  where
    go rmap current target path dist
      | current == target = [(reverse (target : path), dist)]  -- Base case: reached the destination
      | otherwise = concatMap (\(nextCity, nextDist) ->
                                go (removeCity current rmap) nextCity target (current : path) (dist + nextDist))
                              (adjacent rmap current)

-- é assim eu pedi ao chatgpt para fazer a allPaths, eu expliquei o que queria que ele fizesse
-- mas lá no fundo ele é que escreveu :/
-- parece correto mas não sei bem como verificar
-- pedi para fazer uma função allPaths que a cada passo ia acumulando distance 
-- e dava um roadmap com todas as edges da cidade atual removidas (porque seria inútil voltar atrás)

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap city1 city2 =
    let possiblities = allPaths roadmap city1 city2
        minVal = minimum [totalDist | (path, totalDist) <- possiblities]
    in [path | (path, totalDist) <- possiblities , totalDist == minVal]

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

gTest4 :: RoadMap
gTest4 = concat (replicate 10000 gTest1) -- bigass graph
