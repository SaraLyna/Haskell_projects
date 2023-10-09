type Point = (Float, Float)
type Path  = [Point]


alterne :: [a] -> [a]
alterne [] = []           
alterne [x] = [x]        
alterne (x:_:xs) = x : alterne xs


combine :: (a -> b -> c) -> [a] -> [b] -> [c]
combine _ [] _ = []             
combine _ _ [] = []           
combine f (x:xs) (y:ys) = f x y : combine f xs ys

pasPascal :: [Integer] -> [Integer]
pasPascal prevRow = zipWith (+) (0:prevRow) (prevRow ++ [0])


pascal :: [[Integer]]
pascal = iterate pasPascal [1]


pointAintercaler :: Point -> Point -> Point
pointAintercaler (xA, yA) (xB, yB) =
  ((xA + xB) / 2 + (yB - yA) / 2, (yA + yB) / 2 + (xA - xB) / 2)
  
  
pasDragon :: Path -> Path
pasDragon [] = []
pasDragon [p] = [p]
pasDragon (p1:p2:ps) = p1 : pointAintercaler p1 p2 : pasDragon (p2:ps)


dragon :: Point -> Point -> [Path]
dragon p1 p2 = iterate pasDragon [p1, p2]


dragonOrdre :: Point -> Point -> Int -> Path
dragonOrdre p1 p2 0 = [p1, p2]  
dragonOrdre p1 p2 n =
  let midPoint = pointAintercaler p1 p2
      lowerDragon = dragonOrdre p1 midPoint (n - 1)
      upperDragon = dragonOrdre midPoint p2 (n - 1)
  in lowerDragon ++ upperDragon
  
  
main :: IO ()
main = display
  (InWindow "Courbe du Dragon" (800, 800) (10, 10))
  white
  (Line dragonPath)
  where
    p1 = (50, 400)  -- Point de départ
    p2 = (750, 400) -- Point d'arrêt
    dragonOrder = 12 -- Ordre du dragon
    dragonPath = dragonOrdre p1 p2 dragonOrder
