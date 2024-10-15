import qualified Data.List
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

cities :: RoadMap -> [City]
cities roadmap = Data.List.nub [pair | (city1, city2, _) <- roadmap, pair <- [city1, city2]]

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadmap city1 city2 = or [(src, dest) == (city1, city2) || (src, dest) == (city2, city1) | (src, dest, _) <- roadmap]

distance :: [(City, City, Distance)] -> City -> City -> Maybe Distance
distance roadmap city1 city2 = 
        case Data.List.find (\(src, dest, _) -> (src == city1 && dest == city2) || (src == city2 && dest == city1)) roadmap of
        Nothing -> Nothing
        Just (_, _, dist) -> Just dist

adjacent :: RoadMap -> City -> [(City,Distance)]
--adjacent roadmap city = [(dest, dist)| (src, dest, dist) <- roadmap, city == src] ++ [(src, dist)| (src, dest, dist) <- roadmap, city == dest] -- é O(2n)

adjacent roadmap city = [if city == src then (dest, dist) 
                         else (src, dist) 
                         | (src, dest, dist) <- roadmap, city == src || city == dest] -- não está tão clean, mas é O(n)

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance roadmap (city1:city2:path) = 
    case distance roadmap city1 city2 of
    Nothing -> Nothing
    Just dist -> case pathDistance roadmap (city2:path) of
        Nothing -> Nothing
        Just dist_rest -> Just(dist + dist_rest)

rome :: RoadMap -> [City] -- estava a pensar correr a lista cities criar tipo [(City, Int)] e depois voltar a correr e adicionar o counter cada vez que encontramos a cidade, no final damos print das cidades com o maior counter
rome = undefined

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

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
