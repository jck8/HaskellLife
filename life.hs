module Main where

import Data.List
import System.IO
import System.Environment
import System.Random

toad = [(5, 5), (6, 5), (7, 5), (4, 6), (5, 6), (6, 6)]
beacon = [(0, 0), (0, 1), (1, 0), (2, 3), (3, 3), (3, 2)]
glider = [(10, 2), (11, 2), (12, 2), (12, 1), (11, 0)]

shift (x, y) = map (\(a, b) -> (a + x, b + y))

gliders = shift (15, 15) beacon `union` glider `union` toad

inc x dim = (x + 1) `mod` dim
dec x dim = (x - 1) `mod` dim

neighbors (x, y) (width, height) =
  [(dec x width, inc y height),    (x, inc y height),     (inc x width, inc y height),      
  (dec x width, y),                                       (inc x width, y),
  (dec x width, dec y height),     (x, dec y height),     (inc x width, dec y height)]

newCell life dims cell
  |num == 3 = True
  |num == 2 && cell `elem` life = True
  |otherwise = False
    where num = length $ filter (==True) neighborsAlive
          neighborsAlive = map (`elem` life) (neighbors cell dims)

update life (width, height) = filter (newCell life (width, height)) [(x, y)|x<-[0..width], y<-[0..height]]

showCell life cell
  |cell `elem` life = "O"
  |otherwise = "-"

showRow life width row = concatMap (showCell life) [(x, row)|x<-[0..width]]
               
showLife life (width, height) = map (showRow life width) [0..height]  

disp life dims = mapM putStrLn (showLife life dims)

loop life dims = do
  disp life dims
  getLine
  loop (update life dims) dims

randomLife::(RandomGen a) => Int -> a -> (Int, Int) -> [(Int, Int)]
randomLife n gen (width, height) = take n $ zip (randomRs (0, width) gen) (drop n $ randomRs (0, height) gen)
  
main = do
  gen <- getStdGen
  fileName <- getArgs
  handle <- openFile (fileName !! 0) ReadMode
  contents <- hGetContents handle
  let dims = read $ head (lines contents)
      life = read $ (lines contents) !! 1 :: [(Int, Int)]
      rLife = randomLife 300 gen dims
  loop life dims