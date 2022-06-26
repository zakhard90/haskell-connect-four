{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use second" #-}
module Main where

-- [x] Game board
-- [x] Inputs 
-- [x] Animations and movement restrictions
-- [x] Draw grid
-- [x] Game state
-- [x] Display text
-- [ ] Place piece action
-- [ ] Alternate player turns
-- [ ] Connect 4 / win
-- [ ] Game over
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game

data Game = Game { cursorPosition :: (Float, Float)
                 , isPlayersTurn :: Bool
                 , playerPieces :: [(Float, Float)]
                 , opponentPieces :: [(Float, Float)]
                 , isWinner :: Bool
                 , isGameOver :: Bool
                 }
  deriving (Read, Show)

step :: Float
step = 30

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

posArray :: Int -> Int -> [(Float, Float)]
posArray cols rows =
  [(x, y)
  | x <- map (* step) [fromIntegral firstCol .. fromIntegral lastCol]
  , y <- map (* step) [fromIntegral firstRow .. fromIntegral lastRow]]
  where
    lastCol = div (cols - 1) 2

    firstCol = negate lastCol

    lastRow = 0

    firstRow = negate $ rows - 1

gameGrid :: Int -> Int -> Picture -> Picture
gameGrid c r p = Pictures $ repeatBlank p $ posArray c r

refGrid :: Int -> Int -> Picture -> Picture
refGrid c r p = Pictures $ repeatBlank p $ posArray c r

initGame :: Game
initGame = Game { cursorPosition = (0, roof)
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
  | otherwise = Pictures
    [ gameGrid 7 6 (Color (greyN 0.92) $ ThickCircle 4 10)
    , refGrid 7 6 (Color (greyN 0.95) $ Circle $ radius + 4)
    , uncurry cursor $ cursorPosition game
    , displayPlayer game]

boundedWidth :: Float -> Float
boundedWidth x
  | x < leftEdge = leftEdge
  | x > rightEdge = rightEdge
  | otherwise = x

boundedHeight :: Float -> Float
boundedHeight y
  | y < ground = ground
  | otherwise = y

gravityPull :: Float -> Float
gravityPull y
  | y < speed = roof
  | otherwise = y - speed

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
  game { cursorPosition = ( fst $ cursorPosition game
                          , boundedHeight (snd (cursorPosition game) - step))
       }
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) game =
  game { cursorPosition = ( boundedWidth (fst (cursorPosition game) + step)
                          , snd $ cursorPosition game)
       }
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) game =
  game { cursorPosition = ( boundedWidth (fst (cursorPosition game) - step)
                          , snd $ cursorPosition game)
       }
inputHandler _ game = game

runGame :: Float -> Game -> Game
runGame _ game
  | isGameOver game = game
  | isWinner game = game
  | otherwise =
    game { cursorPosition = ( fst $ cursorPosition game
                            , gravityPull $ snd $ cursorPosition game)
         }
