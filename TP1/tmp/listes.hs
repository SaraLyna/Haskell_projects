Prelude> [1, 2, 3]
[1,2,3]
it :: Num t => [t]
Prelude> [True, False]
[True,False]
it :: [Bool]
Prelude> [(1,2),(3,4)]
[(1,2),(3,4)]
it :: (Num t, Num t1) => [(t1, t)]

Prelude> [True, False] !! 1
False
it :: Bool
Prelude> "abcdef" !! 3
'd'
it :: Char
