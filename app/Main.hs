{-# OPTIONS_GHC -Wall #-}

import System.Random
import Data.List (intercalate, minimumBy)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

-- Our cell types including enemies and loot
data Cell = Wall | Empty | Enemy | Loot deriving (Eq, Show)

-- Grid is a 2D list of cells
type Grid = [[Cell]]
type Position = (Int, Int)
type MapSeed = Int
type Probability = Double
-- Helper function to split list into chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Convert random number to cell using guards
randomToCell :: Probability -> Double -> Cell
randomToCell wallProbability r
    | r < wallProbability = Wall
    | otherwise = Empty

-- Generate random grid with given wall probability
generateRandomGrid :: Probability -> MapSeed -> Int -> Int -> Grid
generateRandomGrid wallProbability  seed width height =
    take height (chunksOf width (map (randomToCell wallProbability) (randoms (mkStdGen seed))))

-- Show single cell
showCell :: Cell -> Char
showCell Wall = '#'
showCell Empty = ' '
showCell Enemy = '*'
showCell Loot = '$'

-- Show single row
showRow :: [Cell] -> String
showRow row = map showCell row

-- Pretty print the grid
showGrid :: Grid -> String
showGrid grid = intercalate "\n" (map showRow grid)

-- Check if position is out of bounds
isOutOfBounds :: Position -> Int -> Int -> Bool
isOutOfBounds (x, y) width height = x < 0 || x >= width || y < 0 || y >= height

-- Safe access to list element
safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:_) 0 = Just x
safeIndex (_:xs) n
    | n < 0     = Nothing
    | otherwise = safeIndex xs (n - 1)

-- Get cell at position, treating out-of-bounds as walls
getCellAt :: Grid -> Position -> Int -> Int -> Cell
getCellAt grid (x, y) width height
    | isOutOfBounds (x, y) width height = Wall
    | otherwise = case safeIndex grid y of
        Nothing -> Wall
        Just row -> case safeIndex row x of
            Nothing -> Wall
            Just cell -> cell

-- Generate all 8 neighbor positions
getNeighborPositions :: Position -> [Position]
getNeighborPositions (x, y) =
    filter (/= (x, y)) (concatMap (\dx -> map (\dy -> (x + dx, y + dy)) [-1, 0, 1]) [-1, 0, 1])

-- Check if cell is wall
isWall :: Cell -> Bool
isWall Wall = True
isWall _ = False

-- Count wall neighbors for a given position
countWallNeighbors :: Grid -> Position -> Int -> Int -> Int
countWallNeighbors grid pos width height =
    length (filter isWall (map (\neighborPos -> getCellAt grid neighborPos width height) (getNeighborPositions pos)))

-- Apply cellular automata rule using guards
applyRule :: Int -> Cell
applyRule wallNeighbors
    | wallNeighbors >= 4 = Wall
    | otherwise = Empty

-- Generate all positions in grid
getAllPositions :: Int -> Int -> [Position]
getAllPositions width height =
    concatMap (\y -> map (\x -> (x, y)) [0 .. width - 1]) [0 .. height - 1]

-- Apply rule to single position
evolvePosition :: Grid -> Position -> Int -> Int -> Cell
evolvePosition grid pos width height = applyRule (countWallNeighbors grid pos width height)

-- Apply cellular automata rules to evolve the grid
evolveGrid :: Grid -> Int -> Int -> Grid
evolveGrid grid width height =
    chunksOf width (map (\pos -> evolvePosition grid pos width height) (getAllPositions width height))

-- Run multiple generations of evolution
evolveGenerations :: Int -> Grid -> Int -> Int -> Grid
evolveGenerations 0 grid _ _ = grid
evolveGenerations n grid width height = evolveGenerations (n - 1) (evolveGrid grid width height) width height

-- Check if cell is empty (suitable for enemy/loot placement)
isEmpty :: Cell -> Bool
isEmpty Empty = True
isEmpty _ = False

-- Get all empty positions from the grid
getEmptyPositions :: Grid -> Int -> Int -> [Position]
getEmptyPositions grid width height =
    filter (\pos -> isEmpty (getCellAt grid pos width height)) (getAllPositions width height)

-- Convert random number to boolean for enemy placement
shouldPlaceEnemy :: Probability -> Double -> Bool
shouldPlaceEnemy probability r = r < probability

-- Convert random number to boolean for loot placement
shouldPlaceLoot :: Probability -> Double -> Bool
shouldPlaceLoot probability r = r < probability

-- Replace cell at index in row 
replaceAtRow :: [Cell] -> Int -> Cell -> [Cell]
replaceAtRow [] _ _ = []
replaceAtRow (_:xs) 0 newCell = newCell : xs
replaceAtRow (x:xs) n newCell
    | n < 0     = x:xs
    | otherwise = x : replaceAtRow xs (n - 1) newCell

-- Replace cell at position in grid
replaceAt :: Grid -> Position -> Cell -> Grid
replaceAt [] _ _ = []
replaceAt (row:rows) (x, 0) newCell = replaceAtRow row x newCell : rows
replaceAt (row:rows) (x, y) newCell = row : replaceAt rows (x, y - 1) newCell

-- Place enemy at position if it should be placed
placeEnemyAt :: Grid -> Position -> Probability -> Double -> Grid
placeEnemyAt grid pos probability randomVal
    | shouldPlaceEnemy probability randomVal = replaceAt grid pos Enemy
    | otherwise = grid

-- Place loot at position if it should be placed
placeLootAt :: Grid -> Position -> Probability -> Double -> Grid
placeLootAt grid pos probability randomVal
    | shouldPlaceLoot probability randomVal = replaceAt grid pos Loot
    | otherwise = grid

-- Place enemies on specific positions 
placeEnemiesOnPositions :: Double -> Grid -> [Position] -> [Double] -> Grid
placeEnemiesOnPositions _ grid [] _ = grid
placeEnemiesOnPositions _ grid _ [] = grid
placeEnemiesOnPositions probability grid (pos:positions) (r:randoms) =
    let grid' = placeEnemyAt grid pos probability r
    in placeEnemiesOnPositions probability grid' positions randoms

-- Place loot on specific positions
placeLootOnPositions :: Probability -> Grid -> [Position] -> [Double] -> Grid
placeLootOnPositions _ grid [] _ = grid
placeLootOnPositions _ grid _ [] = grid
placeLootOnPositions probability grid (pos:positions) (r:randoms) =
    let grid' = placeLootAt grid pos probability r
    in placeLootOnPositions probability grid' positions randoms

-- Place enemies on empty positions using random values
placeEnemies :: Probability -> Grid -> [Double] -> Int -> Int -> Grid
placeEnemies probability grid randoms width height =
    placeEnemiesOnPositions probability grid (getEmptyPositions grid width height) randoms

-- Place loot on empty positions using random values
placeLoot :: Probability -> Grid -> [Double] -> Int -> Int -> Grid
placeLoot probability grid randoms width height =
    placeLootOnPositions probability grid (getEmptyPositions grid width height) randoms

-- Find all regions of connected empty space using BFS.
identifyRegions :: Grid -> Int -> Int -> [[Position]]
identifyRegions grid width height = go Set.empty (getAllPositions width height) []
  where
    go _ [] acc = acc
    go visited (p:ps) acc
        | Set.member p visited = go visited ps acc
        | not (isEmpty (getCellAt grid p width height)) = go visited ps acc
        | otherwise =
            let (region, vis') = bfsRegion p visited
            in go vis' ps (region : acc)
    -- BFS returns all the positions of the region and the updated visited set
    bfsRegion start vis = bfs (Seq.singleton start) (Set.insert start vis) []
      where
        bfs Seq.Empty vis' region = (region, vis')
        bfs (q Seq.:<| qs) vis' region =
            let nbrs = filter (\n -> isEmpty (getCellAt grid n width height) && not (Set.member n vis'))
                               (тeighbors q)
                vis'' = foldr Set.insert vis' nbrs
                qs' = qs Seq.>< Seq.fromList nbrs
            in bfs qs' vis'' (q : region)
        -- Neighborhood (without diagonals)
        тeighbors (x, y) =
            filter (\(nx, ny) -> not (isOutOfBounds (nx, ny) width height))
                [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

-- Creates tunnels between disconnected areas to connect them.
connectRegions :: Grid -> Int -> Int -> Grid
connectRegions grid width height =
    let regions = identifyRegions grid width height
    in case regions of
        [] -> grid
        [r] -> grid
        (r0:rs) -> foldl connect grid rs
          where
            -- For each subsequent area, we look for the closest points to the connected area and build a passage.
            connect g region =
                let target = closestPoints r0 region
                in digTunnel g (fst target) (snd target)
            closestPoints rA rB =
                minimumBy (\((ax,ay),(bx,by)) ((cx,cy),(dx,dy)) ->
                             compare ((ax-bx)^2 + (ay-by)^2) ((cx-dx)^2 + (cy-dy)^2))
                          [((ax,ay),(bx,by)) | (ax,ay) <- rA, (bx,by) <- rB]
            digTunnel g (x1,y1) (x2,y2)
                | (x1, y1) == (x2, y2) = replaceAt g (x1, y1) Empty
                | x1 /= x2  = digTunnel (replaceAt g (x1, y1) Empty) (x1 + signum (x2-x1), y1) (x2, y2)
                | y1 /= y2  = digTunnel (replaceAt g (x1, y1) Empty) (x1, y1 + signum (y2-y1)) (x2, y2)
                | otherwise = g

-- Generate grid with enemies and loot, after ensuring connectivity.
generateGridWithEnemiesAndLoot :: Probability -> Probability -> Probability -> MapSeed -> Int -> Int -> Grid
generateGridWithEnemiesAndLoot wallProbability enemyProb lootProb seed width height =
    placeLoot lootProb gridWithEnemies lootRandoms width height
  where
    baseGrid = generateRandomGrid wallProbability seed width height
    evolvedGrid = evolveGenerations 5 baseGrid width height
    connectedGrid = connectRegions evolvedGrid width height
    enemyRandoms = randoms (mkStdGen (seed + 1000))
    lootRandoms  = randoms (mkStdGen (seed + 2000))
    gridWithEnemies = placeEnemies enemyProb connectedGrid enemyRandoms width height

countEnemies :: Grid -> Int
countEnemies grid = length (filter (== Enemy) (concat grid))

countLoot :: Grid -> Int
countLoot grid = length (filter (== Loot) (concat grid))

readIntSafe :: String -> Maybe Int
readIntSafe s = case reads s of
    [(n, "")] -> Just n
    _         -> Nothing

readDoubleSafe :: String -> Maybe Double
readDoubleSafe s = case reads s of
    [(n, "")] -> Just n
    _         -> Nothing

handleEmptyInput :: String -> IO Int
handleEmptyInput input = case null input of
    True -> randomIO :: IO Int
    False -> handleValidInput input

handleValidInput :: String -> IO Int
handleValidInput input = case readIntSafe input of
    Just n -> return n
    Nothing -> randomIO :: IO Int

handleDimensionInput :: String -> Int -> IO Int
handleDimensionInput input defaultValue = case readIntSafe input of
    Just n -> return n
    Nothing -> return defaultValue

handleProbabilityInput :: String -> Double -> IO Double
handleProbabilityInput input defaultValue = case readDoubleSafe input of
    Just n  -> return n
    Nothing -> return defaultValue

main :: IO ()
main = do
    putStrLn "Enter a seed number (or press Enter for random):"
    input <- getLine
    seed <- handleEmptyInput input

    putStrLn "Enter width of a map (default 50):"
    widthInput <- getLine
    gridWidth <- handleDimensionInput widthInput 50

    putStrLn "Enter height of a map (default 30):"
    heightInput <- getLine
    gridHeight <- handleDimensionInput heightInput 30

    putStrLn "Enter enemy probability (default 0.04):"
    enemyProbInput <- getLine
    enemyProb <- handleProbabilityInput enemyProbInput 0.04

    putStrLn "Enter loot probability (default 0.03):"
    lootProbInput <- getLine
    lootProb <- handleProbabilityInput lootProbInput 0.03

    putStrLn ("\nGrid size: " ++ show gridWidth ++ "x" ++ show gridHeight)
    putStrLn "\nFinal map with enemies (*) and loot ($):"
    let finalGrid = generateGridWithEnemiesAndLoot 0.4 enemyProb lootProb seed gridWidth gridHeight
    putStrLn (showGrid finalGrid)
    putStrLn ("Total enemies placed: " ++ show (countEnemies finalGrid))
    putStrLn ("Total loot placed: " ++ show (countLoot finalGrid))
    putStrLn ("\nSeed used: " ++ show seed ++ " (save this to reproduce the same cave!)")