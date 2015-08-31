import System.IO
import Data.List
import Control.Monad

consAbsoluteValues :: [Int] -> [(Int, Int)]
consAbsoluteValues = map pair
    where pair n = (n, abs n)

getClosestToZero :: [(Int, Int)] -> Int
getClosestToZero []    = 0
getClosestToZero temps = fst $ minimumBy minSnd temps
    where minSnd (t1, absT1) (t2, absT2)
            | absT1 /= absT2 = compare absT1 absT2
            | otherwise     = compare t2    t1

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    -- hPutStrLn stderr "Debug messages..."

    input_line <- getLine
    let n = read input_line :: Int -- the number of temperatures to analyse
    input2 <- getLine
    -- the N temperatures expressed as integers ranging from -273 to 5526
    let temps = take n $ map read (words input2) :: [Int]

    -- Write answer to stdout
    putStrLn . show . getClosestToZero $ consAbsoluteValues temps


