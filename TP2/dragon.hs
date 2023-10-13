import Graphics.Gloss

main :: IO ()
main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnime (50,250) (450,250))

dragonAnime a b t = Line (dragon a b !! (round t `mod` 20))

dragonOrder = 12
         
         
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
  ((xA + xB)/2 + (yB - yA)/2, (yA + yB)/2 + (xA - xB)/2)
  
  
pasDragon :: Path -> Path
pasDragon [] = []
pasDragon [p] = [p]
pasDragon (p1:[p2])=p1 : [pointAintercaler p1 p2 ] ++ [p2]
pasDragon (p1:p2:p3:ps) = p1:a:p2:b:(pasDragon (p3:ps))
	where a=pointAintercaler p1 p2
	      b=pointAintercaler p3 p2 
 


dragon :: Point -> Point -> [Path]
dragon p1 p2 = iterate pasDragon [p1, p2]


dragonOrdre :: Point -> Point -> Int -> Path
dragonOrdre p1 p2 0 = [p1, p2]  
dragonOrdre p1 p2 n =
  let midPoint = pointAintercaler p1 p2
      lowerDragon = dragonOrdre p1 midPoint (n-1)
      upperDragon = dragonOrdre midPoint p2 (n-1)
  in lowerDragon ++ upperDragon
  

