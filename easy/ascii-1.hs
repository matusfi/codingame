import System.IO
import Control.Monad
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map

alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ?"

type PixelBank = [[String]]
type CharMap = Map.Map Char [String]

charMap :: String -> PixelBank -> CharMap
charMap abc pixelBank = Map.fromList $ zip abc pixelBank

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let width = read input_line :: Int
    input_line <- getLine
    let height = read input_line :: Int
    text <- getLine

    rows <- replicateM height $ do
      row <- getLine
      let splitRow = chunksOf width row
      return splitRow

    -- hPutStrLn stderr "Debug messages..."

    -- Write answer to stdout
    putStrLn "anwer"

display :: CharMap -> String -> String
--display chmap text = unlines . blabla
