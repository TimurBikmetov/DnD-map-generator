import CodeWorld

data BOOL
 = T -- ^ Boolean TRUE.
 | F -- ^ Boolean FALSE.
 | BOOL :&&: BOOL -- ^ Logical AND.
 | BOOL :||: BOOL -- ^ Logical OR.
 deriving (Show, Eq)

step :: BOOL -> BOOL
step (F :&&: T) = F
step (T :&&: F) = F
step (T :&&: T) = T
step (F :&&: F) = F

step (F :||: T) = T
step (T :||: F) = T
step (T :||: T) = T
step (F :||: F) = F

step (F :&&: part) = F :&&: step part
step (T :&&: part) = T :&&: step part
step (part :&&: T) = step part :&&: T
step (part :&&: F) = step part :&&:F

step (F :||: part) = F :||: step part
step (T :||: part) = T :||: step part
step (part :||: T) = step part :||: T
step (part :||: F) = step part :||: F

step (T) = T
step (F) = F


steps :: BOOL -> [BOOL]
steps T = [T]
steps F = [F]
steps expr = expr:steps (step expr)


ppSteps :: BOOL -> IO ()
ppSteps T = putStrLn (show T)
ppSteps F = putStrLn(show F)
ppSteps expr = do
 putStrLn (show expr)
 ppSteps (step expr)
 
 
--2.1
data Gate mode = Gate Double Picture


-- | A potentially infinite game universe.
data Universe mode = Universe
  { uGates :: [Gate mode]
  , speed  :: Double
  }
  
-- | Some sample gates (for testing).
sampleGates :: [Gate Absolute]
sampleGates = [(Gate 0 (solidPolygon [(0, 10), (2, 10), (2, 0), (0, 0)]))]
  
{-
renderGate :: Gate -> Picture
renderGate (Gate x pic) = translated x 0 pic

-- | Render a sequence of gates without list comprehensions or zip.
renderGates :: [Gate] -> Picture
renderGates = go 0
  where
    go _ [] = blank
    go n (g:gs) =translated (n * 200) 0 (renderGate g) <> go (n + 1) gs
-} 



renderGate :: Gate Absolute -> Picture
renderGate (Gate x pic) = translated x 0 pic

renderGates :: [Gate Absolute] -> Picture
renderGates [] = blank
renderGates (g:gs) = renderGate g <> renderGates gs



-- | Render the game universe.
{-
renderUniverse :: Universe -> Picture
renderUniverse (Universe gates speed) = renderGates (take 10 gates)
-}
renderUniverse :: Universe Relative -> Picture
renderUniverse (Universe gates speed) = renderGates (take 10 absGates)
 where
  absGates = toAbsoluteGates gates
  
-- | Update the universe by moving  gates
{-
updateUniverse :: Double -> Universe -> Universe
updateUniverse dt (Universe gates speed) = (Universe (moveAllGates dt gates speed) speed)


moveGate :: Double -> Gate -> Double -> Gate
moveGate dx (Gate x pic) speed = Gate (x - dx * speed) pic
  
moveAllGates :: Double -> [Gate] -> Double -> [Gate]
moveAllGates _ [] _ = []
moveAllGates dt (g:gs) speed = moveGate dt g speed : moveAllGates dt gs speed
-}
updateUniverse :: Double -> Universe Relative -> Universe Relative
updateUniverse dt (Universe gates speed) =
 Universe (moveAllGates dt gates speed) speed
  
moveGate :: Double -> Gate Relative -> Double -> Gate Relative
moveGate dx (Gate x pic) speed = Gate (x - dx * speed) pic
  
  
  
  
moveAllGates :: Double -> [Gate Relative] -> Double -> [Gate Relative]
moveAllGates _ [] _ = []
moveAllGates dt (g:gs) speed = moveGate dt g speed : moveAllGates dt gs speed
-- | A gate with relative offset.
--data RelativeGate = RelativeGate Gate
-- | A gate with absolute offset.
--data AbsoluteGate = AbsoluteGate Gate
type RelativeGate = Gate Relative
type AbsoluteGate = Gate Absolute


{-
toAbsoluteGates :: [RelativeGate] -> [AbsoluteGate]
toAbsoluteGates [] = []
toAbsoluteGates [RelativeGate x] = [AbsoluteGate x]
toAbsoluteGates((RelativeGate x):xs) = (AbsoluteGate x):toAbsoluteGates xs
-}
toAbsoluteGates :: [Gate Relative] -> [Gate Absolute]
toAbsoluteGates = go 0
 where
  go _ [] = []
  go acc (Gate relX pic : rest) = Gate absX pic : go absX rest
   where
    absX = acc + relX

data Relative
data Absolute

 

main :: IO()
main = do
 print (step(F :&&: T))
 print (step (F :&&:(F :||: T)))
 print (steps ((F :||: T) :&&: F))
 print (steps (T:&&:F))
 ppSteps ((F :||: T) :&&: F)
 ppSteps (foldl (:&&:) T [T, F, T, F])
 ppSteps (foldl (:||:) F [F, T, F, T])
 ppSteps (foldr (:&&:) T [T, F, T, F])
 ppSteps (foldr (:||:) F [F, T, F, T])