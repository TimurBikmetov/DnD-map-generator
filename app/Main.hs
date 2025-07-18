{-# OPTIONS_GHC -Wall #-}

import System.Random
import Data.List (intercalate, minimumBy)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

-- Our cell types including enemies and two loot types
data Cell = Wall | Empty | Enemy | Loot1 | Loot2 deriving (Eq, Show)

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
generateRandomGrid wallProbability seed width height =
    take height (chunksOf width (map (randomToCell wallProbability) (randoms (mkStdGen seed))))

-- Show single cell
showCell :: Cell -> Char
showCell Wall = '#'
showCell Empty = ' '
showCell Enemy = '*'
showCell Loot1 = '$'
showCell Loot2 = '@'

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

-- Convert random number to boolean for loot1 placement
shouldPlaceLoot1 :: Probability -> Double -> Bool
shouldPlaceLoot1 probability r = r < probability

-- Convert random number to boolean for loot2 placement
shouldPlaceLoot2 :: Probability -> Double -> Bool
shouldPlaceLoot2 probability r = r < probability

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

-- Place loot1 at position if it should be placed
placeLoot1At :: Grid -> Position -> Probability -> Double -> Grid
placeLoot1At grid pos probability randomVal
    | shouldPlaceLoot1 probability randomVal = replaceAt grid pos Loot1
    | otherwise = grid

-- Place loot2 at position if it should be placed
placeLoot2At :: Grid -> Position -> Probability -> Double -> Grid
placeLoot2At grid pos probability randomVal
    | shouldPlaceLoot2 probability randomVal = replaceAt grid pos Loot2
    | otherwise = grid

-- Place enemies on specific positions 
placeEnemiesOnPositions :: Double -> Grid -> [Position] -> [Double] -> Grid
placeEnemiesOnPositions _ grid [] _ = grid
placeEnemiesOnPositions _ grid _ [] = grid
placeEnemiesOnPositions probability grid (pos:positions) (r:randoms) =
    placeEnemiesOnPositions probability updatedGrid positions randoms
  where
    updatedGrid = placeEnemyAt grid pos probability r

-- Place loot1 on specific positions
placeLoot1OnPositions :: Probability -> Grid -> [Position] -> [Double] -> Grid
placeLoot1OnPositions _ grid [] _ = grid
placeLoot1OnPositions _ grid _ [] = grid
placeLoot1OnPositions probability grid (pos:positions) (r:randoms) =
    placeLoot1OnPositions probability updatedGrid positions randoms
  where
    updatedGrid = placeLoot1At grid pos probability r

-- Place loot2 on specific positions
placeLoot2OnPositions :: Probability -> Grid -> [Position] -> [Double] -> Grid
placeLoot2OnPositions _ grid [] _ = grid
placeLoot2OnPositions _ grid _ [] = grid
placeLoot2OnPositions probability grid (pos:positions) (r:randoms) =
    placeLoot2OnPositions probability updatedGrid positions randoms
  where
    updatedGrid = placeLoot2At grid pos probability r

-- Place enemies on empty positions using random values
placeEnemies :: Probability -> Grid -> [Double] -> Int -> Int -> Grid
placeEnemies probability grid randoms width height =
    placeEnemiesOnPositions probability grid emptyPositions randoms
  where
    emptyPositions = getEmptyPositions grid width height

-- Place loot1 on empty positions using random values
placeLoot1 :: Probability -> Grid -> [Double] -> Int -> Int -> Grid
placeLoot1 probability grid randoms width height =
    placeLoot1OnPositions probability grid emptyPositions randoms
  where
    emptyPositions = getEmptyPositions grid width height

-- Place loot2 on empty positions using random values
placeLoot2 :: Probability -> Grid -> [Double] -> Int -> Int -> Grid
placeLoot2 probability grid randoms width height =
    placeLoot2OnPositions probability grid emptyPositions randoms
  where
    emptyPositions = getEmptyPositions grid width height

-- Find all regions of connected empty space
identifyRegions :: Grid -> Int -> Int -> [[Position]]
identifyRegions grid width height = findAllRegions Set.empty (getAllPositions width height) []
  where
    findAllRegions _ [] foundRegions = foundRegions
    findAllRegions visited (pos:remaining) foundRegions
        | Set.member pos visited = findAllRegions visited remaining foundRegions
        | not (isEmpty (getCellAt grid pos width height)) = findAllRegions visited remaining foundRegions
        | otherwise = findAllRegions newVisited remaining (newRegion : foundRegions)
      where
        (newRegion, newVisited) = exploreRegion pos visited
    
    -- Explore a single region starting from a position
    exploreRegion startPos initialVisited = explorePositions [startPos] initialVisited []
      where
        explorePositions [] currentVisited regionPositions = (regionPositions, currentVisited)
        explorePositions (currentPos:remainingToExplore) currentVisited regionPositions
            | Set.member currentPos currentVisited = explorePositions remainingToExplore currentVisited regionPositions
            | not (isEmpty (getCellAt grid currentPos width height)) = explorePositions remainingToExplore currentVisited regionPositions
            | otherwise = explorePositions newToExplore updatedVisited (currentPos : regionPositions)
          where
            validNeighbors = filter isValidEmptyPosition (getCardinalNeighbors currentPos)
            newToExplore = validNeighbors ++ remainingToExplore
            updatedVisited = Set.insert currentPos currentVisited
        
        isValidEmptyPosition pos = not (Set.member pos initialVisited) && 
                                  not (isOutOfBounds pos width height) &&
                                  isEmpty (getCellAt grid pos width height)
            
        -- Get 4-directional neighbors (no diagonals)
        getCardinalNeighbors (x, y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

-- Creates tunnels between disconnected areas to connect them.
connectRegions :: Grid -> Int -> Int -> Grid
connectRegions grid width height = connectAllRegions grid regions
  where
    regions = identifyRegions grid width height
    
    connectAllRegions g [] = g
    connectAllRegions g [r] = g
    connectAllRegions g (r0:r1:rs) = connectAllRegions g' (r0:rs)
        where
            g' = connectToFirstRegion r0 g r1

    
    -- For each subsequent area, we look for the closest points to the connected area and build a passage.
    connectToFirstRegion firstRegion currentGrid region = digTunnel currentGrid startPoint endPoint
      where
        (startPoint, endPoint) = closestPoints firstRegion region
    
    closestPoints rA rB =
        minimumBy compareDistances [((ax,ay),(bx,by)) | (ax,ay) <- rA, (bx,by) <- rB]
      where
        compareDistances ((ax,ay),(bx,by)) ((cx,cy),(dx,dy)) =
            compare ((ax-bx)^2 + (ay-by)^2) ((cx-dx)^2 + (cy-dy)^2)
    
    digTunnel g (x1,y1) (x2,y2)
        | (x1, y1) == (x2, y2) = replaceAt g (x1, y1) Empty
        | x1 /= x2  = digTunnel (replaceAt g (x1, y1) Empty) (x1 + signum (x2-x1), y1) (x2, y2)
        | y1 /= y2  = digTunnel (replaceAt g (x1, y1) Empty) (x1, y1 + signum (y2-y1)) (x2, y2)
        | otherwise = g

-- Generate grid with enemies and both loot types, after ensuring connectivity.
generateGridWithEnemiesAndLoot :: Probability -> Probability -> Probability -> Probability -> MapSeed -> Int -> Int -> Grid
generateGridWithEnemiesAndLoot wallProbability enemyProb loot1Prob loot2Prob seed width height =
    placeLoot2 loot2Prob gridWithLoot1 loot2Randoms width height
  where
    baseGrid = generateRandomGrid wallProbability seed width height
    evolvedGrid = evolveGenerations 5 baseGrid width height
    connectedGrid = connectRegions evolvedGrid width height
    enemyRandoms = randoms (mkStdGen (seed + 108))
    loot1Randoms = randoms (mkStdGen (seed + 108 * 108))
    loot2Randoms = randoms (mkStdGen (seed + 108 * 108 * 108))
    gridWithEnemies = placeEnemies enemyProb connectedGrid enemyRandoms width height
    gridWithLoot1 = placeLoot1 loot1Prob gridWithEnemies loot1Randoms width height

countEnemies :: Grid -> Int
countEnemies grid = length (filter (== Enemy) (concat grid))

countLoot1 :: Grid -> Int
countLoot1 grid = length (filter (== Loot1) (concat grid))

countLoot2 :: Grid -> Int
countLoot2 grid = length (filter (== Loot2) (concat grid))

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

    putStrLn "Enter loot1 probability (default 0.03):"
    loot1ProbInput <- getLine
    loot1Prob <- handleProbabilityInput loot1ProbInput 0.03

    putStrLn "Enter loot2 probability (default 0.02):"
    loot2ProbInput <- getLine
    loot2Prob <- handleProbabilityInput loot2ProbInput 0.02

    putStrLn ("\nGrid size: " ++ show gridWidth ++ "x" ++ show gridHeight)
    putStrLn "\nFinal map with enemies (*), loot1 ($), and loot2 (@):"
    let finalGrid = generateGridWithEnemiesAndLoot 0.35 enemyProb loot1Prob loot2Prob seed gridWidth gridHeight
    putStrLn (showGrid finalGrid)
    putStrLn ("Total enemies placed: " ++ show (countEnemies finalGrid))
    putStrLn ("Total loot1 placed: " ++ show (countLoot1 finalGrid))
    putStrLn ("Total loot2 placed: " ++ show (countLoot2 finalGrid))
    putStrLn ("\nSeed used: " ++ show seed ++ " (save this to reproduce the same map!)")