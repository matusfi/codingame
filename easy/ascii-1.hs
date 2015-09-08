import System.IO
import Control.Monad
import qualified Data.Map.Strict as Map

alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ?"
alphabetEmptyMap = Map.fromList . map (\c -> (c,"")) $ alphabet

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let width = read input_line :: Int
    input_line <- getLine
    let height = read input_line :: Int
    t <- getLine

    replicateM height $ do
        row <- getLine
        --chunksOf width row
        return ()

    -- hPutStrLn stderr "Debug messages..."

    -- Write answer to stdout
    putStrLn "answer"


