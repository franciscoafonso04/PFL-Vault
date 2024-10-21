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

-- os grafos são undirected, por isso a única maneira de não serem strongly connected
-- é se existirem 2 componentes completamente separados
-- por isso um dfs por qualquer node deve ser suficiente para chegar a todos os nodes
-- quando testei no gTest4 foi de 1.6s para 0.6s

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

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap start finish =
    let possiblities = allPaths roadmap start finish [] 0
        minVal = minimum [totalDist | (path, totalDist) <- possiblities]
    in [path | (path, totalDist) <- possiblities , totalDist == minVal]

-- é um bocado ineficiente, a única maneira de melhorar seria parar de procurar mal a totalDist fosse
-- maior que a dist do melhor path, mas para isso temos de saber a dist do melhor path
-- não sei como passar esse valor a meio de uma recursão, só com uma variavel global
-- mas as coisas no haskell sao imutaveis >:(
-- por isso o quão wild é que seria fazer o best path, guardar esse valor e depois fazer todos os outros
-- live laugh love

------------------------------------------------------------------------------------------------------------------------------ 

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

gTest5 :: RoadMap
gTest5 = [("0","1",1),("0","2",1),("0","3",1),("1","4",1),("2","4",1),("3","4",1)]

gTest6 :: RoadMap
gTest6 =
    [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),
     ("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),
     ("1","7",11),("3","5",14),
     ("0","8",12), ("1","3",15), ("4","6",5), ("5","7",6), ("2","4",18),
     ("3","6",4), ("0","2",11), ("7","9",7), ("8","10",5), ("11","12",8),
     ("10","14",10), ("11","15",13), ("12","16",14), ("14","18",19), ("17","19",6),
     ("20","21",12), ("21","22",8), ("20","23",14), ("22","24",3), ("23","25",9),
     ("25","26",5), ("24","27",18), ("26","28",4), ("27","29",15), ("28","30",11),
     ("31","32",19), ("29","33",12), ("30","34",6), ("31","35",8), ("34","36",3),
     ("32","37",17), ("35","38",20), ("36","39",15), ("38","40",11), ("39","41",2),
     ("42","43",14), ("40","44",10), ("41","45",6), ("45","46",3), ("44","47",8),
     ("42","48",12), ("43","49",7), ("46","50",11), ("48","52",9), ("50","53",5),
     ("51","54",14), ("53","55",12), ("52","56",18), ("55","57",10), ("54","58",4),
     ("59","60",2), ("60","61",7), ("58","62",5), ("62","63",3), ("64","65",8),
     ("63","66",9), ("67","68",12), ("66","69",6), ("67","70",15), ("68","71",13),
     ("70","72",18), ("71","73",20), ("72","74",4), ("73","75",17), ("74","76",12),
     ("77","78",5), ("75","79",14), ("76","80",3), ("78","81",9), ("79","82",2),
     ("81","83",15), ("82","84",11), ("85","86",6), ("84","87",8), ("83","88",12),
     ("89","90",20), ("87","91",9), ("88","92",3), ("90","93",11), ("91","94",4),
     ("92","95",13), ("93","96",14), ("94","97",10), ("95","98",8), ("96","99",5),
     ("100","101",17), ("99","102",12), ("101","103",7), ("102","104",20), ("103","105",6),
     ("106","107",14), ("104","108",10), ("105","109",3), ("107","110",11), ("108","111",18),
     ("110","112",9), ("111","113",4), ("112","114",15), ("113","115",2), ("114","116",6),
     ("115","117",19), ("116","118",12), ("119","120",10), ("118","121",13), ("120","122",8),
     ("121","123",11), ("122","124",5), ("123","125",9), ("124","126",3), ("126","127",7),
     ("128","129",20), ("125","130",4), ("127","131",6), ("130","132",10), ("129","133",15),
     ("131","134",17), ("132","135",11), ("134","136",8), ("133","137",14), ("135","138",12),
     ("136","139",2), ("137","140",18), ("139","141",19), ("138","142",3), ("140","143",13),
     ("141","144",6), ("142","145",4), ("143","146",20), ("145","147",9), ("144","148",7),
     ("149","150",12), ("147","151",5), ("150","152",2), ("151","153",11), ("152","154",14),
     ("153","155",18), ("156","157",10), ("154","158",20), ("157","159",8), ("158","160",9),
     ("161","162",6), ("160","163",4), ("163","164",2), ("162","165",12), ("165","166",7),
     ("166","167",11), ("167","168",5), ("168","169",14), ("169","170",10), ("171","172",3),
     ("170","173",15), ("172","174",12), ("173","175",20), ("174","176",8), ("175","177",9),
     ("176","178",19), ("177","179",14), ("178","180",6), ("179","181",4), ("181","182",11),
     ("182","183",5), ("183","184",10), ("180","185",2), ("184","186",12), ("185","187",3),
     ("187","188",8), ("188","189",15), ("189","190",6), ("191","192",20), ("190","193",11),
     ("192","194",2), ("193","195",17), ("194","196",9), ("195","197",10), ("197","198",8),
     ("198","199",4), ("200","201",3), ("201","202",20), ("202","203",6), ("203","204",5),
     ("204","205",19), ("205","206",10), ("206","207",11), ("207","208",15), ("208","209",18),
     ("209","210",12), ("210","211",2), ("211","212",9), ("212","213",4), ("213","214",3),
     ("214","215",20), ("215","216",7), ("216","217",8), ("217","218",12), ("218","219",5),
     ("220","221",14), ("219","222",11), ("221","223",6), ("222","224",19), ("223","225",17),
     ("225","226",3), ("226","227",20), ("227","228",8), ("228","229",10), ("229","230",12),
     ("230","231",11), ("231","232",7), ("232","233",5), ("233","234",19), ("234","235",4),
     ("235","236",15), ("236","237",14), ("237","238",3), ("238","239",18), ("239","240",6),
     ("241","242",9), ("240","243",2), ("242","244",11), ("243","245",20), ("244","246",4),
     ("246","247",15), ("245","248",10), ("247","249",7), ("248","250",3), ("249","251",5),
     ("250","252",12), ("252","253",11), ("253","254",8), ("254","255",19), ("255","256",14),
     ("256","257",6), ("257","258",4), ("258","259",15), ("259","260",10), ("261","262",20),
     ("260","263",12), ("262","264",11), ("263","265",5), ("264","266",3), ("265","267",7),
     ("266","268",8), ("267","269",18), ("269","270",15), ("270","271",10), ("271","272",6),
     ("272","273",4), ("273","274",12), ("274","275",19), ("275","276",3), ("276","277",8),
     ("277","278",11), ("278","279",5), ("279","280",20), ("280","281",6), ("281","282",17),
     ("282","283",14), ("283","284",9), ("284","285",4), ("285","286",18), ("286","287",12),
     ("287","288",11)] -- bigass graph generated by chatgpt
