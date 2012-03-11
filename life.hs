life1 = [(0, 1), (0, 2), (0, 10)] 

countNeighbors (a, b) life = [(a+1, b) `elem` life, (a, b+1) `elem` life, (a+1, b+1) `elem` life, (a-1, b) `elem` life, (a-1, b-1) `elem` life, (a, b-1) `elem` life, (a+1, b-1) `elem` life, (a-1, b+1) `elem` life]

showCell life (x, y)
  |(x, y) `elem` life = "O"
  |otherwise = "-"

showRow life width row = concatMap (showCell life) [(x, row)|x<-[0..width]]
               
showLife life (width, height) = map (showRow life width) [0..height]  

disp life (width, height) = mapM putStrLn (showLife life (width, height))

main = disp life1 (10, 10)