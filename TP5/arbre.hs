import Test.QuickCheck
import Control.Concurrent (threadDelay)

data Arbre coul val = Noeud coul val (Arbre coul val) (Arbre coul val)| Feuille deriving Show


mapArbre :: (coul -> coul) -> (val -> val) -> Arbre coul val -> Arbre coul val 
mapArbre _ _ Feuille = Feuille
mapArbre fc fv (Noeud coul val g d) = Noeud (fc coul) (fv val)  (mapArbre fc fv g) (mapArbre fc fv d)


hauteur :: Arbre coul val -> Int
hauteur Feuille = 0
hauteur (Noeud _ _ g d) = 1 +   (hauteur g) `max` (hauteur d)


taille :: Arbre coul val -> Int
taille Feuille = 0
taille (Noeud _ _ g d) = 1 + taille g + taille d


dimension :: Num a => Arbre coul val -> (a-> a -> a) -> a 
dimension Feuille f = 0
dimension (Noeud _ _ g d) f = 1 + (dimension g f) `f` (dimension d f )



peigneGauche :: [(c,a)] -> Arbre c a
peigneGauche [] = Feuille
peigneGauche ((c,a):cv) = Noeud c a Feuille (peigneGauche cv)



prop_hauteurPeigne xs = length xs == hauteur (peigneGauche xs)



estParfait :: Arbre c a -> Bool
estParfait Feuille = True
--estParfait arbre = hauteur arbre == (2 ^ (taille arbre + 1) - 1)
estparfait (Noeud _ _ g d) = (hauteur g  == hauteur d) && estParfait g && estParfait d


estParfait' :: Arbre c a -> Bool
estParfait' arbre = let h = fromIntegral (hauteur arbre)
                        t = fromIntegral (taille arbre)
                   in (dimension arbre (*) == (2 ^ (t + 1) - 1))
                   

         
parfait :: Int -> [(c, a)] -> Arbre c a
parfait _ [] = Feuille
parfait 0 _ = Feuille
parfait h l= Noeud fst.parfait' snd.parfait' fg fd
    where fg = (parfait (h-1) take h l )
          fd = (parfait (h-1) drop h l)

parfait' :: [b] -> (c, a)
parfait' (x:xs) = (Noeud fg fd, xs)
        where 
            (fg, a)= parfait' xs
            (fd, b)= parfait' a


--a -> [a]


--aplatit :: Arbre c a -> [(c, a)]

--map snd (aplatit parfait4) == "abcdefghijklmno"


--element :: Eq a => a -> Arbre c a -> Bool


--a [color=red, fontcolor=red]

--noeud :: (c -> String) -> (a -> String) -> (c,a) -> String

--arcs :: Arbre c a -> [(a,a)]

--arc :: (a -> String) -> (a,a) -> String

--dotise :: String -> (c -> String) -> (a -> String) -> Arbre c a -> String



--elementR ::



--data Couleur = red | black

--data ArbreRN = Arbre red Char | Arbre black Char

--equilibre :: ArbreRN a -> ArbreRN a


--insertion ::

--arbresDot :: [Char] -> [String]


--main = mapM_ ecrit arbres
--    where ecrit a = do writeFile "arbre.dot" a
--                       threadDelay 1000000
--          arbres  = arbresDot "gcfxieqzrujlmdoywnbakhpvst"