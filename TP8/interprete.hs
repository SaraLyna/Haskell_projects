import Parser
import Data.Maybe
import Data.List
import Control.Monad (liftM, ap)


type Nom = String

data Expression = Lam Nom Expression 
                | App Expression Expression
                | Var Nom
                | Lit Litteral
                deriving (Show,Eq)
                --Lam : la fonction qui associe à une variable (Nom) un corps (Expression) ; le nom de ce constructeur vient du lambda-calcul ;
                --App : l’application d’une fonction à son argument, tous les deux donnés comme des Expressions ;
                --Var : une variable dont le nom est donné ;
                --Lit : un littéral, c’est-à-dire un entier ou un booléen, dans notre langage minimal.

data Litteral = Entier Integer
              | Bool   Bool
              deriving (Show,Eq)



chaine' :: String -> Parser String
chaine' ""     = pure ""
chaine' (c:cs) = car c      >>= \_ ->
                chaine' cs  >>= \_ ->
                pure (c:cs)


chaine1 :: String -> Parser String
chaine1 ""     = pure ""
chaine1 (c:cs) = car c      >>
                chaine1 cs  >>
                pure (c:cs)


chaine2 :: String -> Parser String
chaine2 ""     = pure ""
chaine2 (c:cs) = do car c
                    chaine2 cs
                    pure (c:cs)


some2 :: Parser a -> Parser [a]
some2 p = p      >>= \r ->
         many p >>= \rs ->
         pure (r:rs)


some1 :: Parser a -> Parser [a]
some1 p = do r  <- p
             rs <- many p
             pure (r:rs)


----------------------------------------------------

espacesP :: Parser ()
espacesP =  many(carQuand (`elem` " " )) >> pure ()
--espacesP = many (chaine " ") >> pure ()



nomP :: Parser Nom
nomP = some (carQuand (`elem` (['a'..'z']++['A'..'Z']))) >>= \nom -> espacesP >> pure nom


varP :: Parser Expression
varP = some (carQuand (`elem` (['a'..'z']++['A'..'Z']))) >>= \nom -> espacesP >> pure (Var nom)


applique :: [Expression] -> Expression
applique [e] = e
applique (e1:e2:e3) = applique ((App e1 e2) : e3)



exprP :: Parser Expression
exprP = booleenP <|> varP <|> lambdaP  <|> exprParentheseeP <|> nombreP

exprsP :: Parser Expression
exprsP = (some exprP)  >>= \e -> pure ( applique e)


lambdaP :: Parser Expression
lambdaP = do
        carQuand (`elem` "\\") >> espacesP
        c1 <- carQuand (`elem` (['a'..'z']++['A'..'Z']))
        espacesP >> carQuand (`elem` "-") >> carQuand (`elem` ">") >> espacesP
        c2 <- exprsP
        pure (Lam [c1] (c2))



exprParentheseeP :: Parser Expression
exprParentheseeP = do
                car '('
                c <- exprsP 
                car ')'
                pure(c) 


nombreP :: Parser Expression
nombreP = some (carQuand (`elem` (['0'..'9']))) >>= \nombre -> espacesP >> pure (Lit (Entier(read nombre)))


booleenP :: Parser Expression
booleenP =(chaine "True" <|> chaine "False" )>>= \bool -> pure (Lit (Bool (read bool)))


expressionP :: Parser Expression
expressionP = espacesP >> exprsP


ras :: String -> Expression
ras  string 
    |runParser expressionP string == Just (exp, "") = exp
    |runParser expressionP string == n = error "Erreur d’analyse syntaxique"
    |otherwise = error "Erreur d’analyse syntaxique"
    where 
        Just (exp,_) = runParser expressionP string
        n = runParser expressionP string

--------------------------------------------------------------------------------------------------
--Interprétation
--------------------------------------------------------------------------------------------------

data ValeurA = VLitteralA Litteral
             | VFonctionA (ValeurA -> ValeurA)

instance Show ValeurA where
    show (VFonctionA _) = "λ"
                       -- ^ ou "VFonctionA _", ou "<fun>" ou toute
                       --   autre représentation des fonctions de
                       --   votre choix
    show (VLitteralA (Entier a)) = show a
    show (VLitteralA (Bool True)) = "True"
    show (VLitteralA (Bool False)) = "False"

type Environnement a = [(Nom, a)]


-- envA :: Environnement ValeurA
-- envA = [ ("neg",   negA)
--        , ("add",   releveBinOpEntierA (+))
--        , ("soust", releveBinOpEntierA (-))
--        , ("mult",  releveBinOpEntierA (*))
--        , ("quot",  releveBinOpEntierA quot) ]



interpreteA :: Environnement ValeurA -> Expression -> ValeurA
interpreteA e (Var x) = fromJust (lookup x e)
interpreteA e (Lam x exp) = VFonctionA (\f -> interpreteA ((x, f) : e) exp)
interpreteA e (Lit x) = VLitteralA x
interpreteA e (App exp1 exp2) =VFonctionA (\exp1 -> interpreteA e exp2)

-- negA :: ValeurA
-- negA a = interpreteA 


-- addA :: ValeurA
-- addA


-- releveBinOpEntierA :: (Integer -> Integer -> Integer) -> ValeurA
-- releveBinOpEntierA


-- ifthenelseA :: ValeurA
-- ifthenelseA


----------------------------------------------------------------------------------------
-- data Either a b = Left a
--                  | Right b

-- data ValeurB = VLitteralB Litteral
--               | VFonctionB (ValeurB -> ErrValB)

-- type MsgErreur = String
-- type ErrValB   = Either MsgErreur ValeurB

-- instance Show ValeurB where
--     ....



-- interpreteB :: Environnement ValeurB -> Expression -> ErrValB

-- addB :: ValeurB


-- quotB :: ValeurB



-- main :: IO ()

----------------------------------------------------------------------------------------
data ValeurC = VLitteralC Litteral
             | VFonctionC (ValeurC -> OutValC)

type Trace   = String
type OutValC = (Trace, ValeurC)

-- instance Show ValeurC where
--     ....


-- interpreteC :: Environnement ValeurC -> Expression -> OutValC


-- pingC :: ValeurC


---------------------------------------------------------------------------------------
data ValeurM m = VLitteralM Litteral
               | VFonctionM (ValeurM m -> m (ValeurM m))


-- instance Show ValeurM where
--     ....

-- data SimpleM v = S v
--                deriving Show


-- instance Functor SimpleM where
--     fmap = _

-- instance Applicative SimpleM where
--     pure  = _
--     (<*>) = _

-- instance Monad SimpleM where
--     (>>=) = _

-- fmap  :: (a -> b) -> SimpleM a -> SimpleM b
-- pure  :: a -> SimpleM a
-- (<*>) :: SimpleM (a -> b) -> SimpleM a -> SimpleM b
-- (>>=) :: SimpleM a -> (a -> SimpleM b) -> SimpleM b



-- instance Functor SimpleM where
--     fmap = liftM

-- instance Applicative SimpleM where
--     pure  = S
--     (<*>) = ap

-- instance Monad SimpleM where
--     (S v) >>= f = f v



-- type InterpreteM m = Environnement (ValeurM m) -> Expression -> m (ValeurM m)

-- interpreteS :: InterpreteM SimpleM
-- interpreteS = interpreteM


-- interpreteSimpleM :: Environnement (ValeurM SimpleM) -> Expression -> SimpleM (ValeurM SimpleM)



-- interpreteM :: Monad m => Environnement (ValeurM m) -> Expression -> m (ValeurM m)



-----------------------------------------------------------------------------------------------
-- data TraceM v = T (Trace, v)
--               deriving Show


-- -- interpreteMT :: InterpreteM TraceM
-- -- interpreteMT = interpreteM

-- pingM :: ValeurM TraceM
-- pingM = VFonctionM (\v -> T ("p", v))



-- -- interpreteMT' :: InterpreteM TraceM


-- --------------------------------------------------------------------------------------------
-- data ErreurM v = Succes v
--                | Erreur String
--                deriving Show

-- instance MonadFail ErreurM where
--     fail = Erreur


-- interpreteE :: InterpreteM ErreurM


--------------------------------------------------------------------------------------------