import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let surfacen = read input_line :: Int -- the number of points used to draw the surface of Mars.

    replicateM surfacen $ do
        input_line <- getLine
        let input = words input_line
        let landx = read (input!!0) :: Int -- X coordinate of a surface point. (0 to 6999)
        let landy = read (input!!1) :: Int -- Y coordinate of a surface point. By linking all the points together in a sequential fashion, you form the surface of Mars.
        return ()
    loop

loop :: IO ()
loop = do
    input_line <- getLine
    let input = words input_line
    let x = read (input!!0) :: Int
    let y = read (input!!1) :: Int
    let hspeed = read (input!!2) :: Int -- the horizontal speed (in m/s), can be negative.
    let vspeed = read (input!!3) :: Int -- the vertical speed (in m/s), can be negative.
    let fuel = read (input!!4) :: Int -- the quantity of remaining fuel in liters.
    let rotate = read (input!!5) :: Int -- the rotation angle in degrees (-90 to 90).
    let power = read (input!!6) :: Int -- the thrust power (0 to 4).

    -- hPutStrLn stderr "Debug messages..."


    -- rotate power. rotate is the desired rotation angle. power is the desired thrust power.

    let command = control vspeed power
    setKnobs command

    loop


type Pos = Int
type Speed = Int
type FuelVolume = Int
type Rotation = Int
type Power = Int

data Command = Command Rotation Power
                deriving Show


setKnobs :: Command -> IO ()
setKnobs (Command rotate power) = putStrLn (show rotate ++ " " ++ show power)

decPower :: Power -> Power
decPower p
    | p > 0 = p - 1
    | otherwise = 0

incPower :: Power -> Power
incPower p
    | p < 4 = p + 1
    | otherwise = 4

control :: Speed -> Power -> Command
control    vSpeed   power
    | vSpeed <= (-40) = Command 0 (incPower power)
    | vSpeed >= (-39) = Command 0 (decPower power)
    | otherwise       = Command 0 power
