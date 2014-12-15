import Data.List (minimumBy)
import Control.Monad

type Distance = Int
type ShipName = String

data Enemy = Enemy ShipName Distance
             deriving (Show, Eq)

compareDist :: Enemy -> Enemy -> Ordering
compareDist (Enemy _ d1) (Enemy _ d2) = compare d1 d2

name :: Enemy -> ShipName
name (Enemy n _) = n

dist :: Enemy -> Distance
dist (Enemy _ d) = d

minDist :: Distance
minDist = 1000

inRange :: Distance -> [Enemy] -> [Enemy]
inRange range = filter (\enemy -> range >= (dist enemy))

closest :: [Enemy] -> Enemy
closest =  minimumBy compareDist

loop :: IO ()
loop = do
    input_line <- getLine
    let count = read input_line :: Int -- The number of current enemy ships within range
    
    enemies <- getEnemies count
    --mapM print enemies
    putStrLn . name . closest $ inRange minDist enemies

    loop

getEnemies :: Int -> IO [Enemy]
getEnemies count =
  replicateM count $ do
    input_line <- getLine
    let input = words input_line
    let enemyName = input!!0 -- The name of this enemy
    let enemyDist = read (input!!1) :: Int -- The distance to your cannon of this enemy
    return $ Enemy enemyName enemyDist
