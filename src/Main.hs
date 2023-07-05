import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Parallel.Strategies (parMap, rdeepseq)


-- Constants
width, height, offset :: Int
width = 800 -- Width of the window
height = 600 -- Height of the window
offset = 10 -- Size of each cell

-- Cell data type
type Cell = (Int, Int)

-- Initial configuration
glider :: [Cell]
glider = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]

-- Create an empty grid
emptyGrid :: Int -> Int -> [(Int, Int)]
emptyGrid w h = [(x, y) | x <- [0..w-1], y <- [0..h-1]]

-- Translate logical coordinates to screen coordinates
toScreen :: Int -> Int -> Point
toScreen x y = (fromIntegral x * fromIntegral offset - fromIntegral width / 2, fromIntegral y * fromIntegral offset - fromIntegral height / 2)

-- Translate screen coordinates to logical coordinates
toLogical :: Point -> Cell
toLogical (x, y) = (round ((x + fromIntegral width / 2) / fromIntegral offset), round ((y + fromIntegral height / 2) / fromIntegral offset))

-- Check if a given cell is alive
isAlive :: [Cell] -> Cell -> Bool
isAlive cells cell = cell `elem` cells

-- Count the number of live neighbors for a given cell
countNeighbors :: [Cell] -> Cell -> Int
countNeighbors cells (x, y) = length $ filter (\(x', y') -> abs (x' - x) <= 1 && abs (y' - y) <= 1 && (x', y') /= (x, y)) cells

-- Determine the next state of a given cell
nextState :: [Cell] -> Cell -> Bool
nextState cells cell
  | alive && (neighbors == 2 || neighbors == 3) = True -- Survival
  | not alive && neighbors == 3 = True -- Reproduction
  | otherwise = False -- Death
  where
    alive = isAlive cells cell
    neighbors = countNeighbors cells cell

-- Compute the next generation of cells
-- Compute the next generation of cells in parallel
nextGeneration :: [Cell] -> [Cell]
nextGeneration cells =
  let chunks = splitIntoChunks 1000 (emptyGrid width height) -- Adjust the chunk size as needed
      nextGenChunks = parMap rdeepseq (nextGenerationChunk cells) chunks
  in concat nextGenChunks

-- Compute the next generation for a chunk of cells
nextGenerationChunk :: [Cell] -> [(Int, Int)] -> [Cell]
nextGenerationChunk cells chunk = filter (nextState cells) chunk

-- Split a list into chunks of the given size
splitIntoChunks :: Int -> [a] -> [[a]]
splitIntoChunks _ [] = []
splitIntoChunks n list = chunk : splitIntoChunks n rest
  where
    (chunk, rest) = splitAt n list


-- Update the game state to the next generation
updateGame :: Float -> ([Cell], Bool) -> ([Cell], Bool)
updateGame _ (cells, paused)
  | paused = (cells, paused)  -- If paused, don't update the game
  | otherwise = (nextGeneration cells, paused)

-- Drawing function
drawGame :: ([Cell], Bool) -> Picture
drawGame (cells, _) = pictures $ map drawCell cells
  where
    drawCell (x, y) = translate (fromIntegral x * fromIntegral offset - fromIntegral width / 2) (fromIntegral y * fromIntegral offset - fromIntegral height / 2) $ rectangleSolid (fromIntegral offset) (fromIntegral offset)


-- Handle mouse click events
handleClick :: Event -> ([Cell], Bool) -> ([Cell], Bool)
handleClick (EventKey (MouseButton LeftButton) Down _ mouse) (cells, paused) = (toLogical mouse : cells, paused) -- place cell
handleClick (EventKey (MouseButton RightButton) Down _ _) (cells, paused) = (cells, not paused) -- Toggle pause
handleClick _ game = game


-- Main function
main :: IO ()
main = play
  (InWindow "Conway's Game of Life" (width, height) (100, 100)) -- Window settings
  white -- Background color
  10 -- Frames per second
  (glider,True) -- Initial state
  drawGame -- Drawing function
  handleClick -- Event handling function
  updateGame -- Update function
