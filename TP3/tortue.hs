import Graphics.Gloss

type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]
type EtatTortue = (Point, Float)
type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas
              ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue
type EtatDessin = (EtatTortue, Path)




motSuivant :: Regles -> Mot -> Mot
motSuivant r [] = []
motSuivant r (x:xs) = r x ++ motSuivant r xs

regles :: Symbole -> Mot
regles r
      |r == '+' = ['+']
      |r == '-' = ['-']
      |r == 'F' = "F-F++F-F"
      |otherwise = []



lsysteme :: Axiome -> Regles -> LSysteme
lsysteme [] r = []
lsysteme ax r  = [ax, iterate(\x -> motSuivant x r)ax]


etatInitial :: Config -> EtatTortue
etatInitial (e,_,_,_,_) = e



longueurPas :: Config -> Float
longueurPas (_,l,_,_,_) = l



facteurEchelle :: Config -> Float
facteurEchelle (_,_,f,_,_) = f


angle :: Config -> Float
angle (_,_,_,a,_) = a



symbolesTortue :: Config -> [Symbole]
symbolesTortue (_,_,_,_,s) = s



avance :: Config -> EtatTortue -> EtatTortue
avance (_,d,_,_,_) ((x,y),cap) =  ((x+d*cos(cap),y+d*sin(cap)),cap)



tourneAGauche :: Config -> EtatTortue -> EtatTortue
tourneAGauche (_,_,_,a,_) ((x,y),cap)=((x,y),cap+a)




tourneADroite :: Config -> EtatTortue -> EtatTortue
tourneADroite (_,_,_,a,_) ((x,y),cap)=((x,y),cap-a)


--filtreSymbolesTortue :: Config -> Mot -> Mot
--filtreSymbolesTortue c a = a



--interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
--interpreteSymboles c e = a b


--interpreteMot :: Config -> Mot -> Picture
--interepreteMot c m = a



--lsystemeAnime :: LSysteme -> Config -> Float -> Picture
