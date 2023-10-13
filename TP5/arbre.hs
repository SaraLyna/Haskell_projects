import Test.QuickCheck

data Arbre coul val = Noeud coul val (Arbre coul val) (Arbre coul val)| Feuille deriving Show
mapArbre :: (coul -> coul) -> (val -> val) -> Arbre coul val -> Arbre coul val 
mapArbre _ _ Feuille = Feuille
mapArbre fc fv (Noeud coul val g d) = Noeud (fc coul) (fv val)  (mapArbre fc fv g) (mapArbre fc fv d)


--couleur :: coul -> String 


--valeur :: val -> Int


hauteur :: Arbre coul val -> Integer 
hauteur Feuille = 0
hauteur (Noeud _ _ g d) = 1 +   (hauteur g) `max` (hauteur d)


taille :: Arbre coul val -> Integer
taille Feuille = 0
taille (Noeud _ _ g d) = 1 + taille g + taille d


dimension :: Num a => Arbre coul val -> (a-> a -> a) -> a 
dimension Feuille f = 0
dimension (Noeud _ _ g d) f = 1 + (dimension g f) `f` (dimension d f )




--peigneGauche :: [(c,a)] -> Arbre c a
--peigneGauche [(c,a)] = Noeud c a (peigneGauche [(c,a)] g c a) Feuille



--prop_hauteurPeigne xs = length xs == hauteur (peigneGauche xs)

--estParfait :: Arbre c a -> Bool

--parfait :: Int -> [(c, a)] -> Arbre c a


--aplatit :: Arbre c a -> [(c, a)]


--element :: Eq a => a -> Arbre c a -> Bool

