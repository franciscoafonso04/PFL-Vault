-- APENAS USAR ESTAS FUNÇÕES SE O SHORTESTPATH BFS CORRER MUITO MAL
-- basicamente fiz este ficheiro para dar cleanup no ficheiro principal
-- mal o bfs estiver 100% operacional eliminamos isto

type City = String
type Path = [City]
type Distance = Int
type RoadMap = [(City,City,Distance)]

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

-- Helper Function that removes a city from the roadmap.
--
-- Parameters:
--   city - The city to be removed.
--   roadmap - A list of tuples representing the roads between cities and their distances.
--
-- Returns: A new roadmap that includes all the previous edges except the ones that include the city to be removed.

removeCity :: City -> RoadMap -> RoadMap
removeCity city = filter (\(city1, city2, _) -> city1 /= city && city2 /= city)

------------------------------------------------------------------------------------------------------------------------------

-- Helper Function that generates all possible paths from one city to another.
--
-- Parameters:
--   roadmap - A list of tuples representing the roads between cities and their distances.
--   current - The city we are currently on, the beggining of the traversal.
--   target - The city we want to reach.
--   visited - A list off all of the cities we have visited.
--   totalDist - The distance of the path so far.
--
-- Returns: A list of tuples of all possible paths between current and target, with the total distance of the path.

allPaths :: RoadMap -> City -> City -> [City] -> Distance -> [(Path, Distance)]
allPaths roadmap current target visited totalDist 
    | current == target = [(reverse (current : visited), totalDist)]
    | otherwise = [path | (nextCity, nextDist) <- adjacent roadmap current, 
                   path <- allPaths (removeCity current roadmap) nextCity target (current : visited) (totalDist + nextDist)]

------------------------------------------------------------------------------------------------------------------------------ 

-- Computes all possible shortest paths from start to finish.
--
-- Parameters:
--   roadmap - A list of tuples representing the roads between cities and their distances.
--   start - The city we start our traversal.
--   finish - The city we want to reach.
--   
-- Returns: A list of all possible shortest paths from start to finish.

shortestPath roadmap start finish =
   let possiblities = allPaths roadmap start finish [] 0
       minVal = minimum [totalDist | (path, totalDist) <- possiblities]
   in [path | (path, totalDist) <- possiblities, totalDist == minVal]