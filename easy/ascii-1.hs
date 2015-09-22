import System.IO
import Control.Monad
import Data.Char (toUpper)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map

alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ?"

type PixelChar = [String]
type PixelString = [PixelChar]
type PixelCharMap = Map.Map Char PixelChar

charMap :: String -> PixelString -> PixelCharMap
charMap abc pixelBank = Map.fromList $ zip abc pixelBank

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let width = read input_line :: Int
    input_line <- getLine
    let height = read input_line :: Int
    text <- liftM (map toUpper) getLine

    pixelBank <- liftM transpose $ replicateM height $ do
      row <- getLine
      let splitRow = chunksOf width row
      return splitRow

    -- errorCheck width height pixelBank
    let chmap = charMap alphabet pixelBank


    -- hPutStrLn stderr "Debug messages..."

    -- Write answer to stdout
    putStrLn $ display chmap height text

errorCheck :: Int -> Int -> PixelString -> IO ()
errorCheck width height pixelBank
  | length pixelBank /= height = error "Pixel Bank height doesn't match display height"
  | any (\s -> length s /= width) (concat pixelBank) = error "Pixel Bank character width is not consistent with the expected width"
  | otherwise = return ()

display :: PixelCharMap -> Int -> String -> String
display chmap height text = unlines . joinPixelLines (length text) . replaceWithPixelChars $ text
  where
    choosePixelChar :: Char -> PixelChar
    choosePixelChar = (Map.!) chmap -- (Map.!) is unsafe Map key accessor

    replaceWithPixelChars :: String -> PixelString
    replaceWithPixelChars = map choosePixelChar

    lineNumbers = [0..height-1]

    joinPixelLines :: Int -> PixelString -> [String]
    joinPixelLines len ps = map concat ps


testMap :: PixelCharMap
testMap = Map.fromList [('A', [" A  ",
                               "AAA ",
                               "A A "]),
                        ('H', ["H H ",
                               "HHH ",
                               "H H "])]
