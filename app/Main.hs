{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use second" #-}
module Main where

-- [x] Game board
-- [x] Inputs
-- [x] Animations and movement restrictions
-- [x] Draw grid
-- [x] Game state
-- [x] Display text
-- [x] Place piece action
-- [x] Alternate player turns
-- [ ] Connect 4 / win
-- [ ] Game over
-- [ ] Random opponent
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (sortOn)

data Game = Game
  { cursorPosition :: (Float, Float)
  , isPlayersTurn :: Bool
  , playerPieces :: [(Float, Float)]
  , opponentPieces :: [(Float, Float)]
  , isWinner :: Bool
  , isGameOver :: Bool
  }
  deriving (Read, Show)

step :: Float
step = 30

rows :: Float
rows = 6

cols :: Float
cols = 7

radius :: Float
radius = 10

frames :: Int
frames = 25

speed :: Float
speed = 1

windowWidth :: Int
windowWidth = 210

windowHeight :: Int
windowHeight = 330

roof :: Float
roof = subtract radius $ fromIntegral $ div windowHeight 2

ground :: Float
ground = negate roof

rightEdge :: Float
rightEdge = subtract (step / 2) $ fromIntegral $ div windowWidth 2

leftEdge :: Float
leftEdge = negate rightEdge

main :: IO ()
main = do
  play windowDisplay white frames initGame renderGame inputHandler runGame

windowDisplay :: Display
windowDisplay = InWindow "Window" (windowWidth, windowHeight) (100, 100)

cursor :: Float -> Float -> Picture
cursor x y = translate x y (Circle radius)

repeatBlank :: Picture -> [(Float, Float)] -> [Picture]
repeatBlank piece = foldr (\c -> (:) (uncurry Translate c piece)) [piece]

repeatPiece :: Game -> Picture -> [(Float, Float)] -> [Picture]
repeatPiece game piece [] = []
repeatPiece game piece xs
  | head xs `elem` playerPieces game =
    uncurry Translate (head xs) (Color black piece) :
    repeatPiece game piece (tail xs)
  | head xs `elem` opponentPieces game =
    uncurry Translate (head xs) (Color red piece) :
    repeatPiece game piece (tail xs)
  | otherwise =
    uncurry Translate (head xs) (Color (greyN 0.92) piece) :
    repeatPiece game piece (tail xs)

--foldr (\(x, y) -> (:) (uncurry Translate (x, y) piece)) [piece] posMatrix
posMatrix :: Float -> Float -> [(Float, Float)]
posMatrix cols rows =
  [ (x, y)
  | x <- map (* step) [firstCol .. lastCol]
  , y <- map (* step) [firstRow .. lastRow]
  ]
 where
  lastCol = (cols - 1) / 2

  firstCol = negate lastCol

  lastRow = 0

  firstRow = negate $ rows - 1

gameGrid :: Game -> Float -> Float -> Picture -> Picture
gameGrid game cols rows piece =
  Pictures $ repeatPiece game piece $ posMatrix cols rows

refGrid :: Float -> Float -> Picture -> Picture
refGrid c r p = Pictures $ repeatBlank p $ posMatrix c r

initGame :: Game
initGame =
  Game
    { cursorPosition = (0, roof)
    , isPlayersTurn = True
    , playerPieces = []
    , opponentPieces = []
    , isWinner = False
    , isGameOver = False
    }

renderGame :: Game -> Picture
renderGame game
  | isGameOver game = displayTextCenter "Game over"
  | isWinner game = displayTextCenter "You won!"
  | otherwise =
    Pictures
      [ refGrid cols rows (Color (greyN 0.95) $ Circle $ radius + 4)
      , gameGrid game cols rows $ ThickCircle 4 10
      , uncurry cursor $ cursorPosition game
      , displayPlayer game
      ]

boundedWidth :: Float -> Float
boundedWidth x
  | x < leftEdge = leftEdge
  | x > rightEdge = rightEdge
  | otherwise = x

boundedHeight :: Float -> Float
boundedHeight y
  | y < ground = ground
  | otherwise = y

columnContains :: Float -> [(Float, Float)] -> Bool
columnContains _ [] = False
columnContains x ls
  | x == fst (head ls) = True
  | otherwise = columnContains x (tail ls)

calculateDepth :: Float -> Float
calculateDepth row = negate $ row * step

getNextInColumn :: Float -> [(Float, Float)] -> Float
getNextInColumn _ [] = calculateDepth (rows - 1)
getNextInColumn x ls
  | x == fst (head ls) = snd (head (reverse $ sortOn snd $ filter ((==x).fst) ls)) + step
  | otherwise = getNextInColumn x (tail ls)

placePiece :: Float -> [(Float, Float)] -> [(Float, Float)] -> [(Float, Float)]
placePiece x as ls
  | columnContains x as = (x, getNextInColumn x as) : ls
  | otherwise = (x, calculateDepth (rows - 1)) : ls

displayTextTop :: String -> Picture
displayTextTop s = Translate (-90) 140 $ Scale 0.1 0.1 $ Text s

displayTextCenter :: String -> Picture
displayTextCenter s = Translate (-40) 0 $ Scale 0.1 0.1 $ Text s

displayPlayer :: Game -> Picture
displayPlayer game
  | isPlayersTurn game = displayTextTop "Player 1"
  | otherwise = displayTextTop "Opponent"

inputHandler :: Event -> Game -> Game
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) game =
  game
    { cursorPosition =
        ( fst $ cursorPosition game
        , boundedHeight (snd (cursorPosition game) - step)
        )
    }
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) game =
  game
    { cursorPosition =
        ( boundedWidth (fst (cursorPosition game) + step)
        , snd $ cursorPosition game
        )
    }
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) game =
  game
    { cursorPosition =
        ( boundedWidth (fst (cursorPosition game) - step)
        , snd $ cursorPosition game
        )
    }
inputHandler _ game = game

runGame :: Float -> Game -> Game
runGame _ game
  | isGameOver game = game
  | isWinner game = game
  | otherwise =
    game
      { cursorPosition =
          if snd (cursorPosition game) < speed
            then (fst $ cursorPosition game, roof)
            else
              ( fst $ cursorPosition game
              , snd (cursorPosition game) - speed
              )
      , playerPieces =
          if isPlayersTurn game && snd (cursorPosition game) < speed
            then placePiece (fst $ cursorPosition game) (playerPieces game ++ opponentPieces game) (playerPieces game)
            else playerPieces game
      , opponentPieces =
          if not (isPlayersTurn game) && snd (cursorPosition game) < speed
            then placePiece (fst $ cursorPosition game) (playerPieces game ++ opponentPieces game) (opponentPieces game)
            else opponentPieces game
      , isPlayersTurn =
          if snd (cursorPosition game) < speed
            then not $ isPlayersTurn game
            else isPlayersTurn game
      }
