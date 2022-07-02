module Main where

-- [x] Game board
-- [x] Inputs
-- [x] Animations and movement restrictions
-- [x] Draw grid
-- [x] Game state
-- [x] Display text
-- [x] Place piece action
-- [x] Alternate player turns
-- [x] Connect 4 / win
-- [x] Game over
-- [x] Read config from file

import Data.List (group, sortOn)
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (mkStdGen, randomR)

main :: IO ()
main = do
  configString <- readFile "Game.conf"
  play
    windowDisplay
    white
    frames
    (initGame $ initConfig configString)
    renderGame
    movementHandler
    runGame

newtype Config = Config {speed :: Float}
  deriving (Read, Show)

data Game = Game
  { config :: Config
  , cursorPosition :: (Float, Float)
  , isPlayersTurn :: Bool
  , playerPieces :: [(Float, Float)]
  , opponentPieces :: [(Float, Float)]
  , isWinner :: Bool
  , isGameOver :: Bool
  }
  deriving (Read, Show)

-- Init external configurations

initConfig :: String -> Config
initConfig s = Config $ read s

-- Static configurations

step :: Float
step = 50

rows :: Float
rows = 6

cols :: Float
cols = 7

radius :: Float
radius = step / 3

frames :: Int
frames = 25

windowWidth :: Float
windowWidth = cols * step

windowHeight :: Float
windowHeight = (rows + 5) * step

roof :: Float
roof = subtract radius $ windowHeight / 2

ground :: Float
ground = negate roof

rightEdge :: Float
rightEdge = subtract (step / 2) $ windowWidth / 2

leftEdge :: Float
leftEdge = negate rightEdge

lastCol :: Float
lastCol = (cols - 1) / 2

firstCol :: Float
firstCol = negate lastCol

lastRow :: Float
lastRow = 0

firstRow :: Float
firstRow = negate $ rows - 1

-- UI elements

windowDisplay :: Display
windowDisplay = InWindow "Window" (round windowWidth, round windowHeight) (100, 100)

cursor :: Float -> Float -> Picture
cursor x y = translate x y (Circle radius)

displayTextTop :: String -> Picture
displayTextTop s = Translate (negate $ step * 3) (step * 5) $ Scale 0.1 0.1 $ Text s

displayTextCenter :: String -> Picture
displayTextCenter s = Translate (-35) 0 $ Scale 0.1 0.1 $ Text s

displayPlayer :: Game -> Picture
displayPlayer game
  | isPlayersTurn game = displayTextTop "Player 1"
  | otherwise = displayTextTop "Opponent"

repeatBlank :: Picture -> [(Float, Float)] -> [Picture]
repeatBlank piece = foldr (\c -> (:) (uncurry Translate c piece)) [piece]

repeatPiece :: Game -> Picture -> [(Float, Float)] -> [Picture]
repeatPiece game piece [] = []
repeatPiece game piece ls@(x : xs)
  | x `elem` playerPieces game =
    uncurry Translate x (Color black piece) :
    repeatPiece game piece xs
  | x `elem` opponentPieces game =
    uncurry Translate x (Color red piece) :
    repeatPiece game piece xs
  | otherwise =
    uncurry Translate x (Color (greyN 0.92) piece) :
    repeatPiece game piece xs

posMatrix :: Float -> Float -> [(Float, Float)]
posMatrix cols rows =
  [ (x, y)
  | x <- map (* step) [firstCol .. lastCol]
  , y <- map (* step) [firstRow .. lastRow]
  ]

gameGrid :: Game -> Float -> Float -> Picture -> Picture
gameGrid game cols rows piece =
  Pictures $ repeatPiece game piece $ posMatrix cols rows

refGrid :: Float -> Float -> Picture -> Picture
refGrid c r p = Pictures $ repeatBlank p $ posMatrix c r

-- Piece placement

placePiece :: Float -> [(Float, Float)] -> [(Float, Float)] -> [(Float, Float)]
placePiece x as ls
  | columnContains x as = (x, getNextInColumn x as) : ls
  | otherwise = (x, calculateDepth (rows - 1)) : ls

columnContains :: Float -> [(Float, Float)] -> Bool
columnContains _ [] = False
columnContains n ls@(x : xs)
  | n == fst x = True
  | otherwise = columnContains n xs

calculateDepth :: Float -> Float
calculateDepth row = negate $ row * step

getNextInColumn :: Float -> [(Float, Float)] -> Float
getNextInColumn _ [] = calculateDepth (rows - 1)
getNextInColumn n ls@(x : xs)
  | n == fst x = snd (last (sortOn snd $ filter ((== n) . fst) ls)) + step
  | otherwise = getNextInColumn n xs

boundedWidth :: Float -> Float
boundedWidth x
  | x < leftEdge = leftEdge
  | x > rightEdge = rightEdge
  | otherwise = x

boundedHeight :: Float -> Float
boundedHeight y
  | y < ground = ground
  | otherwise = y

-- Winning sequence algorythm

checkWin :: [(Float, Float)] -> Bool
checkWin [] = False
checkWin xs
  | length xs < 4 = False
  | otherwise = checkWinColumn xs || checkWinRow xs || checkWinDiagRight xs || checkWinDiagLeft xs

checkWinColumn :: [(Float, Float)] -> Bool
checkWinColumn [] = False
checkWinColumn ls = checkWinColumnValues [firstCol .. lastCol] ls

checkWinColumnValues :: [Float] -> [(Float, Float)] -> Bool
checkWinColumnValues _ [] = False
checkWinColumnValues [] ls = False
checkWinColumnValues ks@(x : xs) ls
  | longestStreak >= 4 = True
  | otherwise = checkWinColumnValues xs ls
 where
  longestStreak = getLongestStreak $ reduceToRowValue $ filterByColumn x ls

checkWinRow :: [(Float, Float)] -> Bool
checkWinRow [] = False
checkWinRow ls = checkWinRowValues [firstRow .. lastRow] ls

checkWinRowValues :: [Float] -> [(Float, Float)] -> Bool
checkWinRowValues _ [] = False
checkWinRowValues [] ls = False
checkWinRowValues ks@(x : xs) ls
  | longestStreak >= 4 = True
  | otherwise = checkWinRowValues xs ls
 where
  longestStreak = getLongestStreak $ reduceToColumnValue $ filterByRow x ls

checkWinDiagRight :: [(Float, Float)] -> Bool
checkWinDiagRight [] = False
checkWinDiagRight ls = (>= 3) $ length $ scanDiagonalRight [firstCol .. lastCol] [firstRow .. lastRow] ls

checkWinDiagLeft :: [(Float, Float)] -> Bool
checkWinDiagLeft [] = False
checkWinDiagLeft ls = (>= 3) $ length $ scanDiagonalLeft (reverse [firstCol .. lastCol]) [firstRow .. lastRow] ls

scanDiagonalRight :: [Float] -> [Float] -> [(Float, Float)] -> [(Float, Float)]
scanDiagonalRight [] _ _ = []
scanDiagonalRight ks@(x : xs) rs ls
  | (>= 3) $ length diag = diag
  | otherwise = scanDiagonalRight xs rs ls
 where
  diag = diagonalRight x rs ls

diagonalRight :: Float -> [Float] -> [(Float, Float)] -> [(Float, Float)]
diagonalRight _ [] _ = []
diagonalRight _ [y] _ = []
diagonalRight _ _ [] = []
diagonalRight x (y : ys) ls
  | hasNextRight = getThis ++ diagonalRight (x + 1) ys ls
  | otherwise = diagonalRight x ys ls
 where
  hasNextRight = (> 0) $ length getNextRight
  getNextRight = getByCoordinates (x + 1) (y + 1) ls
  getThis = getByCoordinates x y ls

scanDiagonalLeft :: [Float] -> [Float] -> [(Float, Float)] -> [(Float, Float)]
scanDiagonalLeft [] _ _ = []
scanDiagonalLeft ks@(x : xs) rs ls
  | (>= 3) $ length diag = diag
  | otherwise = scanDiagonalLeft xs rs ls
 where
  diag = diagonalLeft x rs ls

diagonalLeft :: Float -> [Float] -> [(Float, Float)] -> [(Float, Float)]
diagonalLeft _ [] _ = []
diagonalLeft _ [y] _ = []
diagonalLeft _ _ [] = []
diagonalLeft x (y : ys) ls
  | hasNextLeft = getThis ++ diagonalLeft (x - 1) ys ls
  | otherwise = diagonalLeft x ys ls
 where
  hasNextLeft = (> 0) $ length getNextLeft
  getNextLeft = getByCoordinates (x - 1) (y + 1) ls
  getThis = getByCoordinates x y ls

getByCoordinates :: Float -> Float -> [(Float, Float)] -> [(Float, Float)]
getByCoordinates _ _ [] = []
getByCoordinates x y ls = filterByRow y $ filterByColumn x ls

getLongestStreak :: [Float] -> Int
getLongestStreak [] = 0
getLongestStreak ls = maximum . map length . group . zipWith (-) [1 ..] $ map (/ step) ls

reduceToColumnValue :: [(Float, Float)] -> [Float]
reduceToColumnValue = map fst

reduceToRowValue :: [(Float, Float)] -> [Float]
reduceToRowValue = map snd

filterByColumn :: Float -> [(Float, Float)] -> [(Float, Float)]
filterByColumn x [] = []
filterByColumn x ls = sortOn snd $ filter ((== x * step) . fst) ls

filterByRow :: Float -> [(Float, Float)] -> [(Float, Float)]
filterByRow x [] = []
filterByRow x ls = sortOn fst $ filter ((== x * step) . snd) ls

-- Keyboard input handling

movementHandler :: Event -> Game -> Game
movementHandler e game
  | isPlayersTurn game = inputHandler e game
  | otherwise = inputHandler e game -- TODO AI Handler

inputHandler :: Event -> Game -> Game
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) game = moveFromIndex 1 game
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) game = moveFromIndex 2 game
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) game = moveFromIndex 3 game
inputHandler _ game = game

moveFromIndex :: Int -> Game -> Game
moveFromIndex i game
  | i == 1 =
    game
      { cursorPosition =
          ( fst $ cursorPosition game
          , boundedHeight (snd (cursorPosition game) - step)
          )
      }
  | i == 2 =
    game
      { cursorPosition =
          ( boundedWidth (fst (cursorPosition game) + step)
          , snd $ cursorPosition game
          )
      }
  | i == 3 =
    game
      { cursorPosition =
          ( boundedWidth (fst (cursorPosition game) - step)
          , snd $ cursorPosition game
          )
      }
  | otherwise = game

-- Game state initialization

initGame :: Config -> Game
initGame c =
  Game
    { config = c
    , cursorPosition = (0, roof)
    , isPlayersTurn = True
    , playerPieces = []
    , opponentPieces = []
    , isWinner = False
    , isGameOver = False
    }

-- Game frame rendering

renderGame :: Game -> Picture
renderGame game
  | isGameOver game = displayTextCenter "Game over"
  | isWinner game = displayTextCenter "Well done!"
  | otherwise =
    Pictures
      [ refGrid cols rows (Color (greyN 0.95) $ Circle $ radius + 4)
      , gameGrid game cols rows $ ThickCircle (radius / 2) radius
      , uncurry cursor $ cursorPosition game
      , displayPlayer game
      ]

-- Game execution

runGame :: Float -> Game -> Game
runGame t game
  | isGameOver game = game
  | isWinner game = game
  | otherwise =
    game
      { cursorPosition =
          if snd (cursorPosition game) < speed (config game)
            then (fst $ cursorPosition game, roof)
            else
              ( fst $ cursorPosition game
              , snd (cursorPosition game) - speed (config game)
              )
      , playerPieces =
          if isPlayersTurn game && snd (cursorPosition game) < speed (config game)
            then placePiece (fst $ cursorPosition game) (playerPieces game ++ opponentPieces game) (playerPieces game)
            else playerPieces game
      , opponentPieces =
          if not (isPlayersTurn game) && snd (cursorPosition game) < speed (config game)
            then placePiece (fst $ cursorPosition game) (playerPieces game ++ opponentPieces game) (opponentPieces game)
            else opponentPieces game
      , isPlayersTurn =
          if snd (cursorPosition game) < speed (config game)
            then not $ isPlayersTurn game
            else isPlayersTurn game
      , isGameOver =
          checkWin (opponentPieces game)
      , isWinner =
          checkWin (playerPieces game)
      }
