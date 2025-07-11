-- Cellular Automata Map Generation with Enemies and Loot
-- Pure functional version without let/in/if/then/else
import System.Random
import Data.List (intercalate)

-- Our cell types including enemies and loot
data Cell = Wall | Empty | Enemy | Loot deriving (Eq, Show)

-- Grid is a 2D list of cells
type Grid = [[Cell]]
type Position = (Int, Int)

-- Grid dimensions
gridWidth, gridHeight :: Int
gridWidth = 60
gridHeight = 30

-- Enemy spawn probability (adjust as needed)
enemyProbability :: Double
enemyProbability = 0.04

-- Loot spawn probability (adjust as needed)
lootProbability :: Double
lootProbability = 0.03

-- Helper function to split list into chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Convert random number to cell using guards
randomToCell :: Double -> Double -> Cell
randomToCell wallProbability r
    | r < wallProbability = Wall
    | otherwise = Empty

-- Generate random grid with given wall probability
generateRandomGrid :: Double -> Int -> Grid
generateRandomGrid wallProbability seed =
    take gridHeight (chunksOf gridWidth (map (randomToCell wallProbability) (randoms (mkStdGen seed))))

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
isOutOfBounds :: Position -> Bool
isOutOfBounds (x, y) = x < 0 || x >= gridWidth || y < 0 || y >= gridHeight

-- Safe access to list element
safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:_) 0 = Just x
safeIndex (_:xs) n
    | n < 0     = Nothing
    | otherwise = safeIndex xs (n - 1)

-- Get cell at position, treating out-of-bounds as walls
getCellAt :: Grid -> Position -> Cell
getCellAt grid (x, y)
    | isOutOfBounds (x, y) = Wall
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
countWallNeighbors :: Grid -> Position -> Int
countWallNeighbors grid pos =
    length (filter isWall (map (getCellAt grid) (getNeighborPositions pos)))

-- Apply cellular automata rule using guards
applyRule :: Int -> Cell
applyRule wallNeighbors
    | wallNeighbors >= 4 = Wall
    | otherwise = Empty

-- Generate all positions in grid
getAllPositions :: [Position]
getAllPositions =
    concatMap (\y -> map (\x -> (x, y)) [0 .. gridWidth - 1]) [0 .. gridHeight - 1]

-- Apply rule to single position
evolvePosition :: Grid -> Position -> Cell
evolvePosition grid pos = applyRule (countWallNeighbors grid pos)

-- Apply cellular automata rules to evolve the grid
evolveGrid :: Grid -> Grid
evolveGrid grid =
    chunksOf gridWidth (map (evolvePosition grid) getAllPositions)

-- Run multiple generations of evolution
evolveGenerations :: Int -> Grid -> Grid
evolveGenerations 0 grid = grid
evolveGenerations n grid = evolveGenerations (n - 1) (evolveGrid grid)

-- Check if cell is empty (suitable for enemy/loot placement)
isEmpty :: Cell -> Bool
isEmpty Empty = True
isEmpty _ = False

-- Get all empty positions from the grid
getEmptyPositions :: Grid -> [Position]
getEmptyPositions grid =
    filter (\pos -> isEmpty (getCellAt grid pos)) getAllPositions

-- Convert random number to boolean for enemy placement
shouldPlaceEnemy :: Double -> Double -> Bool
shouldPlaceEnemy probability r = r < probability

-- Convert random number to boolean for loot placement
shouldPlaceLoot :: Double -> Double -> Bool
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

-- Place enemy at position if it should be placed (uses replaceAt)
placeEnemyAt :: Grid -> Position -> Double -> Grid
placeEnemyAt grid pos randomVal
    | shouldPlaceEnemy enemyProbability randomVal = replaceAt grid pos Enemy
    | otherwise = grid

-- Place loot at position if it should be placed (uses replaceAt)
placeLootAt :: Grid -> Position -> Double -> Grid
placeLootAt grid pos randomVal
    | shouldPlaceLoot lootProbability randomVal = replaceAt grid pos Loot
    | otherwise = grid

-- Place enemies on specific positions (uses recursion)
placeEnemiesOnPositions :: Grid -> [Position] -> [Double] -> Grid
placeEnemiesOnPositions grid [] _ = grid
placeEnemiesOnPositions grid _ [] = grid
placeEnemiesOnPositions grid (pos:positions) (r:randoms) =
    placeEnemiesOnPositions (placeEnemyAt grid pos r) positions randoms

-- Place loot on specific positions (uses recursion)
placeLootOnPositions :: Grid -> [Position] -> [Double] -> Grid
placeLootOnPositions grid [] _ = grid
placeLootOnPositions grid _ [] = grid
placeLootOnPositions grid (pos:positions) (r:randoms) =
    placeLootOnPositions (placeLootAt grid pos r) positions randoms

-- Place enemies on empty positions using random values
placeEnemies :: Grid -> [Double] -> Grid
placeEnemies grid randoms =
    placeEnemiesOnPositions grid (getEmptyPositions grid) randoms

-- Place loot on empty positions using random values
placeLoot :: Grid -> [Double] -> Grid
placeLoot grid randoms =
    placeLootOnPositions grid (getEmptyPositions grid) randoms

-- Generate grid with enemies and loot
generateGridWithEnemiesAndLoot :: Double -> Int -> Grid
generateGridWithEnemiesAndLoot wallProbability seed =
    placeLoot gridWithEnemies lootRandoms
    where
        baseGrid = generateRandomGrid wallProbability seed
        evolvedGrid = evolveGenerations 5 baseGrid
        enemyRandoms = randoms (mkStdGen (seed + 1000))
        lootRandoms = randoms (mkStdGen (seed + 2000))
        gridWithEnemies = placeEnemies evolvedGrid enemyRandoms

-- Count enemies in grid
countEnemies :: Grid -> Int
countEnemies grid = length (filter (== Enemy) (concat grid))

-- Count loot in grid
countLoot :: Grid -> Int
countLoot grid = length (filter (== Loot) (concat grid))

-- Safe read function using pattern matching
readIntSafe :: String -> Maybe Int
readIntSafe s = case reads s of
    [(n, "")] -> Just n
    _         -> Nothing

-- Handle empty input
handleEmptyInput :: String -> IO Int
handleEmptyInput input = case null input of
    True -> randomIO :: IO Int
    False -> handleValidInput input

-- Handle valid input
handleValidInput :: String -> IO Int
handleValidInput input = case readIntSafe input of
    Just n -> return n
    Nothing -> randomIO :: IO Int

-- Main function
main :: IO ()
main = do
    putStrLn "Enter a seed number (or press Enter for random):"
    input <- getLine

    seed <- handleEmptyInput input

    putStrLn ("Using seed: " ++ show seed)

    putStrLn "\nInitial random grid:"
    putStrLn (showGrid (generateRandomGrid 0.4 seed))

    putStrLn ("\nGrid size: " ++ show gridWidth ++ "x" ++ show gridHeight)

    putStrLn "\nTesting neighbor counting at position (5,5):"
    putStrLn ("Wall neighbors: " ++ show (countWallNeighbors (generateRandomGrid 0.4 seed) (5, 5)))

    putStrLn "\nAfter 1 generation:"
    putStrLn (showGrid (evolveGrid (generateRandomGrid 0.4 seed)))

    putStrLn "\nAfter 5 generations:"
    putStrLn (showGrid (evolveGenerations 5 (generateRandomGrid 0.4 seed)))

    putStrLn "\nFinal map with enemies (*) and loot ($):"
    let finalGrid = generateGridWithEnemiesAndLoot 0.4 seed
    putStrLn (showGrid finalGrid)
    putStrLn ("Total enemies placed: " ++ show (countEnemies finalGrid))
    putStrLn ("Total loot placed: " ++ show (countLoot finalGrid))

    putStrLn ("\nSeed used: " ++ show seed ++ " (save this to reproduce the same cave!)")
