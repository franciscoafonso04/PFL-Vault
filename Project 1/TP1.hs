{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}

import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

------------------------------------------------------------------------------------------------------------------------------

type City = String
type Path = [City]
type Distance = Int
type RoadMap = [(City,City,Distance)]
type AdjMatrix = Data.Array.Array (Int,Int) (Maybe Distance)
type Visited = Integer

infinite :: Distance
infinite = 100000000

-----------------------------------------------------------------------------------------------------------------------------  

-- Extracts unique cities from a RoadMap, a list of (city1, city2, distance) tuples, representing roads between cities.
--
-- Parameters:
--   roadmap - A list of tuples, where each tuple represents a road with two cities and a distance.
--
-- Returns: A list of cities appearing in the roadmap, with duplicates removed by using Data.List.nub.

cities :: RoadMap -> [City]
cities roadmap = Data.List.nub [cities | (city1, city2, _) <- roadmap, cities <- [city1, city2]]

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

------------------------------------------------------------------------------------------------------------------------------

-- BFS helper function to explore paths layer-by-layer, tracking cumulative distance.
-- 
-- Parameters:
--   r - A list of tuples representing the roads between cities and their distances.
--   s - The city we start our traversal.
--   f - The city we want to reach.
--   queue - A list of (Path, Distance) pairs representing paths to explore and their current distances.
--   paths - A list of all shortest paths found so far with the minimum distance.
--   minDist - The minimum distance for any path to the finish found so far.
--
-- Returns: A list of all paths that reach finish with the shortest possible distance.

bfs :: RoadMap -> City -> City -> [(Path, Distance)] -> [Path] -> Maybe Distance -> [Path]
bfs r s f [] paths _ = Data.List.nub paths  -- Return all found shortest paths when the queue is empty.
bfs r s f ((path, dist):queue) paths minDist
    | current == f = -- When we reach the finish city, check if this path is among the shortest.
        case minDist of
            Nothing -> bfs r s f queue (path : paths) (Just dist)  -- First path to finish sets minDist.
            Just m  -> if dist == m
                        then bfs r s f queue (path : paths) (Just m)  -- Add path if it matches the min distance.
                        else bfs r s f queue paths (Just m)  -- Ignore if distance equals min distance.
    | otherwise = bfs r s f(queue ++ validNextPaths) paths minDist  -- Continue exploring other paths.
    where

    current = last path  -- Current city is the last city in the path.
    validNextPaths = [(path ++ [nextCity], dist + nextDist) | -- Generate valid next paths by adding neighboring cities  
                                                                -- that are not already visited in the current path.
                        (nextCity, nextDist) <- adjacent r current,
                        nextCity `notElem` path]  -- Avoid cycles by ensuring nextCity is not already in path.

------------------------------------------------------------------------------------------------------------------------------

-- Computes all possible shortest paths from start to finish.
--
-- Parameters:
--   roadmap - A list of tuples representing the roads between cities and their distances.
--   start - The city we start our traversal.
--   finish - The city we want to reach.
--   
-- Returns: A list of all possible shortest paths from start to finish.

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap start finish
    | start == finish = [[start]]  -- If start == finish, the shortest path is the city itself.
    | otherwise = bfs roadmap start finish [([start], 0)] [] Nothing
  
------------------------------------------------------------------------------------------------------------------------------

-- Constructs an adjacency matrix for the Traveling Salesman Problem (TSP) representation.
--
-- Parameters:
--   roadmap - A representation of the roadmap consisting of cities and distances between them.
--
-- Returns: 
--   An adjacency matrix where each element (i, j) contains the distance between cities i and j.
--   If there is no direct road between the cities, the value is Nothing.

tspMatrix :: RoadMap -> AdjMatrix
tspMatrix roadmap = Data.Array.array bounds matrix
  where
    citiesList = cities roadmap
    nCities = length citiesList
    bounds = ((0, 0), (nCities - 1, nCities - 1))
    matrix = [((i, j), distance roadmap (show i) (show j)) | i <- [0..(nCities - 1)], j <- [0..(nCities - 1)]]

------------------------------------------------------------------------------------------------------------------------------

-- Generates a bitmask indicating all cities have been visited.
--
-- Parameters:
--   cityCount - The total number of cities in the roadmap.
--
-- Returns:
--   A bitmask where all bits are set to 1, representing that all cities have been visited.

allCitiesVisited :: Int -> Visited
allCitiesVisited cityCount = (1 `Data.Bits.shiftL` cityCount) - 1

------------------------------------------------------------------------------------------------------------------------------

-- Checks if a specific city has been visited using a bitwise AND operation.
--
-- Parameters:
--   bit - The current bitmask representing visited cities.
--   cityIndex - The index of the city to check.
--
-- Returns:
--   A boolean value indicating whether the specified city has been visited (True) or not (False).

isVisited :: Visited -> Int -> Bool
isVisited bit cityIndex = (bit Data.Bits..&. (1 `Data.Bits.shiftL` cityIndex)) /= 0

------------------------------------------------------------------------------------------------------------------------------

-- Updates the bitmask to mark a specific city as visited using a bitwise OR operation.
--
-- Parameters:
--   bit - The current bitmask representing visited cities.
--   cityIndex - The index of the city to mark as visited.
--
-- Returns:
--   The updated bitmask with the specified city marked as visited.

visitCity :: Visited -> Int -> Visited
visitCity bit cityIndex = bit Data.Bits..|. (1 `Data.Bits.shiftL` cityIndex)

------------------------------------------------------------------------------------------------------------------------------
-- Recursively finds the shortest path in the adjacency matrix using a depth-first search approach.
--
-- Parameters:
--   matrix - The adjacency matrix representing distances between cities.
--   visited - A bitmask representing the cities that have been visited so far.
--   index - The current city index being visited.
--   allCitiesVisit - A bitmask representing all cities that need to be visited.
--   path - The current path being explored.
--
-- Returns:
--   A tuple containing the total distance of the shortest path and the corresponding path taken.

findShortestPath :: AdjMatrix -> Visited -> Int -> Visited -> Path -> (Distance, Path)
findShortestPath matrix visited index allCitiesVisit path
    | visited == allCitiesVisit = 
        case matrix Data.Array.! (index, 0) of 
            Just dist -> (dist, reverse (show index : path)) 
            Nothing ->  (100000000, []) 
    | otherwise = 
        let ((_, _), (maxCity, _)) = Data.Array.bounds matrix
            distancePaths = [ (dist + newDist, newCityPath) | nextCity <- [0..maxCity], not (isVisited visited nextCity),
                          let dist = case matrix Data.Array.! (index, nextCity) of
                                        Just d -> d
                                        Nothing -> infinite, 
                          let (newDist, newCityPath) = findShortestPath matrix (visitCity visited nextCity) nextCity allCitiesVisit (show index : path)] 
            (minDist, minPath) = minimum distancePaths
        in  (minDist, minPath) 

------------------------------------------------------------------------------------------------------------------------------

-- Solves the Traveling Salesman Problem (TSP) by finding the shortest tour that visits all cities.
--
-- Parameters:
--   roadmap - A representation of the roadmap consisting of cities and distances between them.
--
-- Returns:
--   A path representing the shortest tour that visits all cities and returns to the starting city.
--   If the roadmap is not strongly connected, returns an empty list.

travelSales :: RoadMap ->  Path
travelSales roadmap
    | not (isStronglyConnected roadmap)  = [] 
    | otherwise = completeTour ++ ["0"]
    where 
        matrix = tspMatrix roadmap
        visitedMask = visitCity (1 `Data.Bits.shiftL` 0) 0
        totalCities = length (cities roadmap)
        visited = allCitiesVisited totalCities
        completeTour =  snd (findShortestPath matrix visitedMask 0 visited []) 

------------------------------------------------------------------------------------------------------------------------------
    
tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

gTest4 :: RoadMap -- bipartite graph, good for checking shortestPath from 0 to 4
gTest4 = [("0","1",1),("0","2",1),("0","3",1),("1","4",1),("2","4",1),("3","4",1)]

gTest5 :: RoadMap -- bigass graph
gTest5 = [("0", "34", 11), ("24", "35", 7), ("35", "6", 11), ("3", "49", 16), ("13", "40", 19), ("19", "32", 23), ("34", "7", 6), ("14", "23", 14), ("47", "6", 23), ("3", "43", 22), ("10", "46", 2), ("20", "35", 20), ("11", "47", 24), ("0", "35", 14), ("0", "47", 13), ("14", "36", 9), ("19", "3", 1), ("6", "43", 24), ("3", "34", 22), ("43", "17", 3), ("13", "17", 14), ("19", "35", 12), ("3", "33", 22), ("10", "43", 3), ("0", "46", 7), ("47", "22", 7), ("19", "20", 2), ("16", "37", 18), ("10", "1", 5), ("12", "25", 3), ("13", "41", 11), ("10", "30", 6), ("22", "3", 17), ("11", "2", 1), ("13", "3", 20), ("33", "15", 9), ("35", "14", 25), ("3", "18", 8), ("13", "26", 7), ("38", "3", 6), ("26", "12", 22), ("20", "4", 22), ("43", "38", 10), ("17", "8", 4), ("36", "17", 16), ("2", "29", 4), ("11", "1", 16), ("3", "44", 14), ("21", "47", 1), ("25", "17", 14), ("25", "35", 13), ("18", "12", 25), ("37", "43", 14), ("37", "6", 18), ("48", "1", 12), ("7", "20", 24), ("3", "46", 1), ("10", "36", 4), ("2", "5", 8), ("30", "25", 19), ("47", "4", 2), ("47", "12", 24), ("19", "48", 17), ("26", "4", 1), ("17", "10", 5), ("46", "19", 4), ("17", "47", 17), ("8", "47", 9), ("14", "22", 10), ("13", "2", 14), ("29", "6", 6), ("43", "7", 24), ("8", "5", 25), ("48", "30", 24), ("6", "28", 16), ("18", "28", 2), ("24", "12", 1), ("20", "3", 22), ("16", "43", 4), ("29", "22", 3), ("9", "35", 14), ("25", "37", 14), ("6", "33", 7), ("7", "6", 6), ("14", "5", 3), ("8", "36", 14), ("19", "23", 4), ("22", "6", 24), ("20", "5", 10), ("7", "9", 10), ("38", "8", 14), ("4", "30", 14), ("30", "41", 16), ("1", "14", 25), ("25", "6", 22), ("48", "40", 1), ("19", "18", 25), ("21", "26", 10), ("47", "1", 18), ("20", "26", 10), ("10", "15", 24), ("1", "46", 3), ("15", "47", 17), ("40", "4", 9), ("15", "13", 1), ("47", "0", 18), ("3", "0", 1), ("11", "14", 22), ("7", "5", 8), ("21", "40", 4), ("30", "8", 10), ("4", "1", 18), ("43", "46", 23), ("22", "4", 14), ("43", "28", 23), ("26", "14", 13), ("10", "8", 1), ("23", "10", 1), ("23", "3", 19), ("21", "23", 1), ("6", "48", 25), ("47", "9", 4), ("45", "47", 25), ("45", "22", 25), ("46", "25", 3), ("10", "13", 18), ("44", "1", 2), ("30", "26", 25), ("6", "32", 19), ("35", "25", 13), ("16", "7", 5), ("6", "46", 25), ("15", "9", 10), ("0", "32", 5), ("40", "47", 16), ("7", "46", 10), ("47", "32", 18), ("43", "36", 3), ("4", "6", 5), ("12", "7", 10), ("11", "36", 17), ("35", "12", 15), ("29", "38", 5), ("41", "3", 15), ("25", "40", 13), ("0", "41", 7), ("46", "9", 5), ("43", "41", 18), ("36", "29", 10), ("43", "32", 8), ("8", "0", 23), ("24", "45", 23), ("48", "47", 23), ("0", "31", 14), ("24", "41", 15), ("22", "9", 16), ("1", "19", 4), ("17", "45", 18), ("29", "17", 13), ("37", "38", 9), ("37", "41", 2), ("43", "3", 8), ("35", "11", 17), ("6", "27", 22), ("13", "36", 6), ("23", "2", 18), ("48", "24", 16), ("48", "3", 10)]

gTest6 :: RoadMap -- bigass graph, kinda bad
gTest6 = concat (replicate 10000 gTest1)



