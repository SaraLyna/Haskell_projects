type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]
type EtatTortue = (Point, Float)
type Config = (EtatTortue 
              ,Float     
              ,Float     
              ,Float      
              ,[Symbole]) 
type EtatDessin = (EtatTortue, Path)




motSuivant :: Regles -> Mot -> Mot
motSuivant r [] = []
motSuivant r (x:xs) = r x ++ motSuivant r xs
 

lsysteme :: Axiome -> Regles -> LSysteme
lsysteme [] r = []
lsysteme ax r = [ax, iterate(motSuivant r ax)]
              
              
etatInitial :: Config -> EtatTortue
etatInitial ((a,b),c,d,e,f) = (a,b)



longueurPas :: Config -> Float
longueurPas ((a,b),c,d,e,f) = c



facteurEchelle :: Config -> Float
facteurEchelle ((a,b),c,d,e,f) = d


angle :: Config -> Float
angle ((a,b),c,d,e,f) = e



symbolesTortue :: Config -> [Symbole]
symbolesTortue ((a,b),c,d,e,f) = f



avance :: Config -> EtatTortue -> EtatTortue
avance a((,b),c,d,e,f) (a,b)= (a,b)



tourneAGauche :: Config -> EtatTortue -> EtatTortue
tourneAGauche a((,b),c,d,e,f) (a,b)= (a,b)




tourneADroite :: Config -> EtatTortue -> EtatTortue
tourneADroite a((,b),c,d,e,f) (a,b)= (b,a)


filtreSymbolesTortue :: Config -> Mot -> Mot
filtreSymbolesTortue a((,b),c,d,e,f) x = y



interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
interpreteSymboles a((,b),c,d,e,f) (a,b) x =(a,b)


interpreteMot :: Config -> Mot -> Picture
interepreteMot a((,b),c,d,e,f) x



lsystemeAnime :: LSysteme -> Config -> Float -> Picture

