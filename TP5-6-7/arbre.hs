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
--peigneGauche = foldr(\(c,a) acc -> Noeud c a acc Feuille) Feuille


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
parfait h l = (fst.parfait' h) l 


parfait' :: Int -> [(c, a)] -> (Arbre c a , [(c,a)])
parfait' 0 e = (Feuille,e)
parfait' h l = (Noeud y z fg fd, b)
        where 
            (fg,(a:as))= parfait' (h-1) l
            (fd, b)= parfait' (h-1) as
            (y,z)=a


infini ::  a -> [a]
infini e = e : infini e



unicode :: [a] -> [((),a)]
unicode [] = []
unicode (x:xs) = ((),x): unicode xs


 
aplatit :: Arbre c a -> [(c, a)]
aplatit Feuille = []
aplatit (Noeud c a fg fd) = aplatit fg ++ [(c,a)] ++ aplatit fd

--map snd (aplatit parfait4) == "abcdefghijklmno"


element :: Eq a => a -> Arbre c a -> Bool
element _ Feuille = False
element e (Noeud _ a fg fd) = element  e fg || element e fd || e==a


--a [color=red, fontcolor=red]

noeud :: (c -> String) -> (a -> String) -> (c,a) -> String
noeud colorToSTring valueToString (c,a) = "\"" ++ valueToString a ++ "\" [color= "++ colorToSTring c ++", label\""++ valueToString a ++ "\"];"

arcs :: Arbre c a -> [(a,a)]
arcs (Noeud _ a g d) = arcs g ++ [(a, valueDroite d)] ++ arcs d
        where 
         valueDroite Feuille = a
         valueDroite (Noeud _ val _ _) = val



arc :: (a -> String) -> (a,a) -> String
arc valueToString (s,d) = " \"" ++ valueToString s ++ "\" -> \"" ++valueToString d ++ "\";"



dotise :: String -> (c -> String) -> (a -> String) -> Arbre c a -> String
dotise nom colorToSTring valueToString arbre = " arbre " ++ nom ++ "{\n" ++ " node[shape=circle]; \n" ++ unlines (map(noeud colorToSTring valueToString) (aplatit arbre)) ++ unlines(map(arc valueToString)(arcs arbre)) ++ "}"



--elementR ::



data Couleur = Red | Black deriving (Show, Eq)

--data ArbreRN = Arbre red Char | Arbre black Char

--equilibre :: ArbreRN a -> ArbreRN a


--insertion ::

--arbresDot :: [Char] -> [String]


--main = mapM_ ecrit arbres
--    where ecrit a = do writeFile "arbre.dot" a
--                       threadDelay 1000000
--          arbres  = arbresDot "gcfxieqzrujlmdoywnbakhpvst"