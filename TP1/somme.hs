somme :: [Int] -> Int
somme [] = 0          -- La somme d'une liste vide est 0
somme (x:xs) = x + somme xs
