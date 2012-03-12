toad = [(5, 5), (6, 5), (7, 5), (4, 6), (5, 6), (6, 6)] 
glider = [(0, 2), (1, 2), (2, 2), (2, 1), (1, 0)]
beacon = [(0, 0), (0, 1), (1, 0), (2, 3), (3, 3), (3, 2)]

countNeighbors (a, b) life = length $ filter (\x -> x==True) [(a+1, b) `elem` life, (a, b+1) `elem` life, (a+1, b+1) `elem` life, (a-1, b) `elem` life, (a-1, b-1) `elem` life, (a, b-1) `elem` life, (a+1, b-1) `elem` life, (a-1, b+1) `elem` life]

newCell life (x, y)
  |num == 3 = True
  |num == 2 && (x, y) `elem` life = True
  |otherwise = False
    where num = countNeighbors (x, y) life

showCell life (x, y)
  |(x, y) `elem` life = "O"
  |otherwise = "-"

showRow life width row = concatMap (showCell life) [(x, row)|x<-[0..width]]
               
showLife life (width, height) = map (showRow life width) [0..height]  

disp life (width, height) = mapM putStrLn (showLife life (width, height))

update life (width, height) = filter (newCell life) [(x, y)|x<-[0..width], y<-[0..height]]

loop life (width, height) = do
  disp life (width, height)
  getLine
  loop (update life (width, height)) (width, height)

main = loop beacon (20, 20)