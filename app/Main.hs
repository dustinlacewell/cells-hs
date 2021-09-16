import System.Random
import System.Exit

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Interface.IO.Game as G
import qualified Control.Monad
import Data.List ( zipWith4 )
import Data.Foldable (toList)
import Control.Monad
import Data.Bits

ruleNo = 30

windowWidth = 1400
windowHeight = 900

rows = 100
cols = 101

cellWidth = windowWidth `div` cols
cellHeight = windowHeight `div` rows

window = InWindow "1D Cellular Automata Explorer" (windowWidth, windowHeight) (0, 0)
background = white
fps = 60

type Cell = Bool
type Row = [Cell]
type Grid = [Row]
type RuleFunction = Bool -> Bool -> Bool -> Bool

--- treat three bools as bits of an int
lookupRuleIndex :: Bool -> Bool -> Bool -> Int
lookupRuleIndex a b c =
    let
        a' = if a then 4 else 0
        b' = if b then 2 else 0
        c' = if c then 1 else 0
    in
        a' + b' + c'

--- for given rule number, return function taking three bools and returning one bool
ruleToFunction :: Int -> Bool -> Bool -> Bool -> Bool
ruleToFunction n a b c =
        let index = lookupRuleIndex a b c
        in testBit n index

--- given a Row and index, return the cell and its neighbors
neighborhood :: Row -> Int -> (Bool, Bool, Bool)
neighborhood row index =
    let
        leftIndex = index - 1
        left = if leftIndex < 0 then row !! (length row - 1)  else row !! leftIndex
        cell = row !! index
        rightIndex = index + 1
        right = if rightIndex > (length row - 1) then head row else row !! rightIndex
    in
        (left, cell, right)

--- map a RuleFunction over each cell's neighborhood in a Row
mapRule :: RuleFunction -> Row -> Row
mapRule rule row =
    let
        neighborhoods = map (neighborhood row) [0..length row - 1]
    in
        map (\(a, b, c) -> rule a b c) neighborhoods

--- append a Row to a Grid, ensuring there are only `rows` rows
appendRow :: Grid -> Row -> Grid
appendRow grid row =
    let
        (first:rest) = grid
        newGrid = rest ++ [row]
    in
        if length newGrid == rows
        then newGrid
        else first : newGrid

mkEmptyGrid rows cols = replicate rows (replicate cols False)

-- Create row of length n with a single centered cell set to True
mkCenteredRow :: Int -> Row
mkCenteredRow n =
    let n2 = n `div` 2
    in replicate n2 False ++ [True] ++ replicate (n2 - 1) False

defaultGrid = [mkCenteredRow cols]

-- Draw a cell at the given position
drawCell :: Int -> Int -> Picture
drawCell x y =
    let
        ox = fromIntegral $ negate cols * cellWidth `div` 2 :: Float
        oy = fromIntegral $ negate rows * cellHeight `div` 2 :: Float
        x' = fromIntegral (x * cellWidth) :: Float
        y' = fromIntegral (windowHeight - (y * cellHeight)) :: Float
        w = fromIntegral cellWidth
        h = fromIntegral cellHeight
    in
        translate ox oy
        $ translate x' y'
        $ rectangleSolid w h

-- Draw a cell for each True in the row
drawRow :: Int -> Row -> Picture
drawRow i row =
    let
        -- create list of (x, i) pairs for each True in row
        cells = zip [0..] (toList row)
        -- filter out cells that are not True
        cells' = filter snd cells
        -- map (x, i) pairs to pictures
        pics = map (\(x, _) -> drawCell x i) cells'
    in
        pictures pics

-- Draw a Grid
drawGrid :: Grid -> Picture
drawGrid grid =
    let
        is = [0..length grid - 1]
        rows = map (grid !!) is
        tups = zip is rows
    in
        pictures $ map (uncurry drawRow) tups

data World = World {
    grid :: Grid,
    rule :: RuleFunction
}

--- make random cell
randomCell :: IO Cell
randomCell = do
    n <- randomRIO (0, 1) :: IO Int
    return $ n == 1


--- make random row
randomRow :: Int -> IO Row
randomRow n =
    replicateM n randomCell

--- make random grid
randomGrid :: Int -> Int -> IO Grid
randomGrid rows cols =
    replicateM rows (randomRow cols)


--- handle escape and quit
handleEvent :: Event -> a -> IO a
handleEvent (G.EventKey (G.SpecialKey G.KeyEsc) G.Down _ _) _ = exitSuccess
handleEvent _ c = return c

-- draw background over whole screen
drawBackground :: Picture
drawBackground =
    let
        w = fromIntegral windowWidth
        h = fromIntegral windowHeight
    in
        color background
        $ rectangleSolid w h

-- draw the grid after clearing the screen
draw :: World -> IO Picture
draw (World grid rule) =
    return $ pictures [drawBackground, drawGrid grid]

-- update the world
updateWorld :: Float -> World -> IO World
updateWorld _ (World grid rule) =
    let
        -- produce new row from last row
        newRow = mapRule rule (last grid)
        -- append new row to grid
        newGrid = appendRow grid newRow
    in
        return $ World newGrid rule

nullUpdate _ = return

main :: IO ()
main = do
    let
        row = mkCenteredRow cols
        rule = ruleToFunction ruleNo
        world = World [row] rule
    playIO window background fps world draw handleEvent updateWorld

