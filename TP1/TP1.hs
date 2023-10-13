somme :: [Int] -> Int
somme [] = 0          
somme (x:xs) = x + somme xs

last' :: [a] -> a
last' xs = head (reverse xs)


init' :: [a] -> [a]
init' xs = reverse (tail (reverse xs))

(!!!) :: [a] -> Int -> a
(x:_) !!! 0 = x
(_:xs) !!! n = xs !!! (n - 1)
_ !!! _ = error "Index out of bounds"

(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x:xs) +++ ys = x : (xs +++ ys)


concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs +++ concat' xss


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs



longueur :: [a] -> Int
longueur xs = somme (map (\_ -> 1) xs)


construireListeRec :: (a -> a) -> a -> Int -> [a]
construireListeRec _ _ 0 = []     
construireListeRec f x n = x : construireListeRec f (f x) (n - 1)


construireListe :: (a -> a) -> a -> Int -> [a]
construireListe f x n = take n (iterate f x)


listeEntiers :: Int -> [Int]
listeEntiers n = construireListe (\x -> x + 1) 0 (n + 1)
