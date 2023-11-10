import Parser


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


--varP :: Parser Expression
--varP =


--applique :: [Expression] -> Expression
--applique =


--exprP :: Parser Expression
--exprP =


--exprsP :: Parser Expression
--exprsP =


--lambdaP :: Parser Expression
--lambdaP =


--exprParentheseeP :: Parser Expression
--exprParentheseeP =

--nombreP :: Parser Expression
--nombreP =


--booleenP :: Parser Expression
--booleenP =


--expressionP :: Parser Expression
--expressionP =


--ras :: String -> Expression
--ras 
