module Main where

-- [x] Game board
-- [x] Inputs 
-- [x] Animations and movement restrictions
-- [ ] Draw grid
-- [ ] Place piece action
-- [ ] Alternate player turns
-- [ ] Connect 4
-- [ ] Game over


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type World = (Float, Float)

step :: Float
step = 30

radius :: Float
radius = 15

frames :: Int
frames = 25

speed :: Float
speed = 1

windowWidth :: Int
windowWidth = 210

windowHeight :: Int
windowHeight = 300

roof :: Float
roof = subtract radius $ fromIntegral $ div windowHeight 2

ground :: Float
ground = negate roof

rightEdge :: Float
rightEdge = subtract radius $ fromIntegral $ div windowWidth 2

leftEdge :: Float
leftEdge = negate rightEdge

main :: IO ()
main = do
    play windowDisplay white frames (0, roof) drawingFunc inputHandler updateFunc


windowDisplay :: Display
windowDisplay = InWindow "Window" (windowWidth,windowHeight) (10, 10)

drawingFunc :: World -> Picture
drawingFunc (x, y) = translate x y (Circle radius)

boundedWidth :: Float -> Float
boundedWidth x
  | x < leftEdge = leftEdge
  | x > rightEdge = rightEdge
  | otherwise = x

inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) (x, y) = (x, y - step)
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) (x, y) = (boundedWidth $ x + step, y)
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) (x, y) = (boundedWidth $ x - step, y)
inputHandler _ w = w    

updateFunc :: Float -> World -> World
updateFunc _ (x, y) = (x, gravity y)
  where
    gravity :: Float -> Float
    gravity y
      | y < (ground + speed) = roof
      | y > ground = y - speed
      | otherwise = y + speed
