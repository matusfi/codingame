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

    errorCheck width height pixelBank
    let chmap = charMap alphabet pixelBank

    -- Write answer to stdout
    putStrLn $ display chmap text

errorCheck :: Int -> Int -> PixelString -> IO ()
errorCheck width height pixelBank
  | length (pixelBank!!0) /= height =
      hPutStrLn stderr (concat ["Pixel Bank height ", (show $ length (pixelBank!!0)), ", doesn't match display height ", (show height)])
  | any (\s -> length s /= width) (concat pixelBank) =
      hPutStrLn stderr "Pixel Bank character width is not consistent with the expected width"
  | otherwise = return ()

display :: PixelCharMap -> String -> String
display chmap text = joinPixelChars . transpose $ replaceWithPixelChars text
  where defChar = (Map.!) chmap '?'

        choosePixelChar :: Char -> PixelChar
        choosePixelChar char = Map.findWithDefault defChar char chmap

        replaceWithPixelChars :: String -> PixelString
        replaceWithPixelChars = map choosePixelChar

        joinPixelChars :: PixelString -> String
        joinPixelChars = unlines . map concat

testMap :: PixelCharMap
testMap = Map.fromList [('A', [" A  ",
                               "AAA ",
                               "A A "]),
                        ('H', ["H H ",
                               "HHH ",
                               "H H "]),
                        ('?', ["??? ",
                               " ?? ",
                               " ?  "])]
