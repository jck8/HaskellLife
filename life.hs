{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Data.List
import System.IO
import System.Environment
import System.Random
import System.Console.CmdArgs
import qualified Data.Set as Set
import qualified Graphics.UI.SDL as SDL

shift (x, y) = map (\(a, b) -> (a + x, b + y))

inc x dim = (x + 1) `mod` (dim + 1)
dec x dim = (x - 1) `mod` (dim + 1)

inc' x dim = x + 1
dec' x dim = x - 1

neighbors (x, y) (width, height) =
  [(dec x width, inc y height),    (x, inc y height),     (inc x width, inc y height),      
  (dec x width, y),                                       (inc x width, y),
  (dec x width, dec y height),     (x, dec y height),     (inc x width, dec y height)]

neighborhood dims cell = cell:(neighbors cell dims) 

allCellsNeighborhood dims life = concatMap (neighborhood dims) life

newCell lifeSet dims cell
  |num == 3 = True
  |num == 2 && cell `Set.member` lifeSet = True
  |otherwise = False
    where num = length $ filter (==True) neighborsAlive
          neighborsAlive = map (`Set.member` lifeSet) (neighbors cell dims)

newCell' life dims cell --For Life w/o death
  |cell `elem` life = True
  |num == 3 = True 
  |otherwise = False
    where num = length $ filter (==True) neighborsAlive
          neighborsAlive = map (`elem` life) (neighbors cell dims)
          
update life (width, height) = filter (newCell (Set.fromList life) (width, height)) [(x, y)|x<-[0..width], y<-[0..height]]

showCell life cell
  |cell `elem` life = "O"
  |otherwise = "-"

showRow life width row = concatMap (showCell life) [(x, row)|x<-[0..width]]
               
showLife life (width, height) = map (showRow life width) [0..height]  

disp life dims = mapM putStrLn (showLife life dims)

loop life dims = do
  dispG life dims
  loop (update life dims) dims

dispG life dims = do 
  screen <- SDL.getVideoSurface
  SDL.fillRect screen clearRect aPixel
  drawCells screen dims (800, 800) life
  SDL.flip screen
 where aPixel = SDL.Pixel 0x00000000
       clearRect = Just SDL.Rect{SDL.rectX = 0, SDL.rectY = 0, SDL.rectW = 800, SDL.rectH = 800} 
                                    
drawCell screen (width, height) (screenWidth, screenHeight) (x, y) =
  SDL.fillRect screen (getCellRect (width, height) (screenWidth, screenHeight) (x, y)) aPixel
 where aPixel = SDL.Pixel 0xFFFFFFFF
  
drawCells screen dims screenDims life = 
  mapM (drawCell screen dims screenDims) life

getCellRect (width, height) (screenWidth, screenHeight) (x, y) =
  Just SDL.Rect{SDL.rectX=gx, SDL.rectY=gy, SDL.rectW=(gh-1), SDL.rectH=(gh-1)}
    where gw = screenWidth `div` (width + 1)
          gh = screenHeight `div` (height + 1)
          gx = gw*x
          gy = gh*y

randomLife::(RandomGen a) => Int -> a -> (Int, Int) -> [(Int, Int)]
randomLife n gen (width, height) = take n $ zip (randomRs (0, width) gen) (drop n $ randomRs (0, height) gen)

goRandom args gen = do putStrLn $ show num
                       loop rLife dims
  where rLife = randomLife num gen dims
        dims = getDims (50, 20) (width args, height args)
        num = number args

getDims altDims cmdDims = if cmdDims == cmdDefaults then altDims else cmdDims
  where cmdDefaults = (-1, -1) 

goFile args = do
  handle <- openFile (file args) ReadMode
  contents <- hGetContents handle
  let dims = getDims (read $ head (lines contents)) (width args, height args)
      life = read $ (lines contents) !! 1 :: [(Int, Int)]  
  loop life dims

data Cmds = Cmds {file :: String, 
                  width :: Int, 
                  height :: Int, 
                  number :: Int} 
          deriving (Show, Data, Typeable)

cmds = Cmds{file = "", width = -1, height = -1, number = 300}

main = do
  SDL.init[SDL.InitEverything]
  SDL.setVideoMode 800 800 32 []
  gen <- getStdGen
  args <- cmdArgs cmds
  if file args=="" then goRandom args gen else goFile args