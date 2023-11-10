-- |
-- Module      :  Parser
--
-- BibliothÃ¨que simple dâ€™analyseurs syntaxiques
--
-- Lâ€™entÃªte du module prÃ©cise explicitement ce qui est exportÃ© par le
-- moduleÂ : en particulier le constructeur `MkParser` nâ€™est pas
-- exportÃ©, de sorte quâ€™il nâ€™est pas possible dâ€™utiliser un `Parser`
-- autrement quâ€™en utilisant une des fonctions prÃ©vues Ã  cet effet.

module Parser ( Parser
              , Resultat
              , runParser
              , resultat
              , unCaractereQuelconque
              , carQuand
              , car
              , chaine
              -- rÃ©exporte les fonctions utiles de Control.Applicative
              , (<|>), empty, many, some )
where

import Control.Applicative
import Control.Monad (ap, liftM)

-- | Type des analyseurs syntaxiques (parseurs)
type Resultat a = Maybe (a, String)
data Parser a   = MkParser (String -> Resultat a)

-- | ExÃ©cute un analyseur syntaxique sur une entrÃ©e donnÃ©e comme une
-- chaÃ®ne
runParser :: Parser a -> String -> Resultat a
runParser (MkParser f) = f

-- | Extrait le rÃ©sultat, en cas de rÃ©ussite du parseur
resultat :: Resultat a -> a
resultat (Just (r, _)) = r


-- * Briques de base et combinateurs

-- $index
--
-- Les briques de base (hormis `unCaractereQuelconque`) sont des
-- membres de classes de type trÃ¨s gÃ©nÃ©riques. On les dÃ©finit donc ici
-- en instanciant les classes de type en question.
--
-- Cela permet en particulier dâ€™obtenir automatiquement les
-- implÃ©mentations standard de `>>`, `many`, `some`, etc. ainsi que
-- toutes les fonctions gÃ©nÃ©riques sâ€™appliquant Ã  ces diffÃ©rentes
-- classes de types.
-- Lâ€™instance de `Monad` permet aussi dâ€™utiliser la notation `do`.
-- Voir la documentation des modules `Data.Functor`,
-- `Control.Applicative` et `Control.Monad`.
--
-- Cela signifie en revanche que la documentation gÃ©nÃ©rÃ©e par haddock
-- omet complÃ¨tement ces diffÃ©rentes fonctions, cachÃ©es en rÃ©sumÃ© dans
-- le fait que `Parser` est une instance des classes de type
-- `Applicative`, `Alternative` et `Monad` (entre autres).

-- | Analyseur qui rÃ©ussit avec le premier caractÃ¨re (quelconque) de
-- lâ€™entrÃ©e
unCaractereQuelconque :: Parser Char
unCaractereQuelconque = MkParser f
    where f     "" = Nothing
          f (c:cs) = Just (c, cs)

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    -- | Analyseur qui rÃ©ussit toujours, avec le rÃ©sultat passÃ© en
    -- argument
    pure v = MkParser f
        where f cs = Just (v, cs)

    (<*>) = ap

instance Alternative Parser where
    -- | Analyseur qui Ã©choue toujours
    empty = MkParser (const Nothing)

    -- | Alternative entre deux parseurs
    -- Combinateur qui retourne le rÃ©sultat du premier analyseur si
    -- celui-ci rÃ©ussit, sinon celui du second
    p1 <|> p2 = MkParser f
        where f cs = case runParser p1 cs of
                        Nothing -> runParser p2 cs
                        r       -> r

instance Monad Parser where
    p >>= fp = MkParser f
        where f cs = case runParser p cs of
                        Nothing       -> Nothing
                        Just (r, cs') -> runParser (fp r) cs'


-- * Analyseurs construits avec les briques de base

-- | Analyseur qui rÃ©ussit avec le premiÃ¨re caractÃ¨re sâ€™il vÃ©rifie la
-- condition donnÃ©e
carQuand :: (Char -> Bool) -> Parser Char
carQuand cond = unCaractereQuelconque >>= filtre
    where filtre c | cond c    = pure c
                   | otherwise = empty

-- | Parser qui rÃ©ussit si lâ€™entrÃ©e commence par le caractÃ¨re donnÃ©
car :: Char -> Parser Char
car c = carQuand (c ==)

-- | Parser qui rÃ©ussit si lâ€™entrÃ©e commence par la chaÃ®ne donnÃ©e
chaine :: String -> Parser String
chaine     "" = pure ""
chaine (c:cs) = car c     >>
                chaine cs >>
                pure (c:cs)
