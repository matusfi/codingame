import System.IO
import Control.Monad
import qualified Data.Map.Strict as M

main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE
  loop


loop :: IO ()
loop = do
  (light, thor, energy) <- readState 
  print . cheapestDir $ costOfMove light thor directions
  loop

readState :: IO (Point, Point, Energy)
readState = do
  pointsLine <- getLine
  energyLine <- getLine
  let points = words pointsLine
  let light = (read (points!!0), read (points!!1)) :: (Int,Int)
  let thor  = (read (points!!2), read (points!!3)) :: (Int,Int)
  
  let energy = read energyLine :: Energy
  
  return (light, thor, energy)

type Energy = Int
type Point  = (Int, Int)
type Distance = Double

distance :: Point -> Point -> Distance
distance (a,b) (c,d) =  sqrt (fromIntegral (a - c) ** 2 + fromIntegral (b - d) ** 2)

data Direction 
  = N
  | NE
  | E
  | SE
  | S
  | SW
  | W
  | NW
  deriving (Show, Eq, Ord)

directions :: [Direction]
directions = [N, NE, E, SE, S, SW, W, NW]

move :: Direction -> Point -> Point
move dir (x,y) = case dir of
  N  -> (x  , y-1)
  NE -> (x+1, y-1)
  E  -> (x+1, y  )
  SE -> (x+1, y+1)
  S  -> (x  , y+1)
  SW -> (x-1, y+1)
  W  -> (x-1, y  )
  NW -> (x-1, y-1)

type DistMap = M.Map Distance Direction

costOfMove :: Point -> Point -> [Direction] -> DistMap
costOfMove light thor dirs = M.fromList $ map (\d -> (distance light (move d thor), d)) dirs

cheapestDir :: DistMap -> Direction
cheapestDir costs = snd . M.findMin $ costs
