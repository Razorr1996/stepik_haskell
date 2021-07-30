module Step_3_2_3 where

-- region Task
-- imports

-- endregion

-- region Task
-- code
decode :: ([a] -> t) -> t
decode c = c []

as :: t1 -> (t1 -> t2) -> t2
as xs c = c xs

a :: t1 -> (t1 -> t2) -> t2
a xs c = c xs

number :: Num a => [a] -> a
number = sum

one :: Num a => [a] -> ([a] -> t) -> t
one xs c = c $ 1 : xs

two :: Num a => [a] -> ([a] -> t) -> t
two xs c = c $ 2 : xs

three :: Num a => [a] -> ([a] -> t) -> t
three xs c = c $ 3 : xs

seventeen :: Num a => [a] -> ([a] -> t) -> t
seventeen xs c = c $ 17 : xs

twenty :: Num a => [a] -> ([a] -> t) -> t
twenty xs c = c $ 20 : xs

hundred :: Num a => [a] -> ([a] -> t) -> t
hundred [] c = c [100]
hundred (x : xs) c = c $ (x * 100) : xs

thousand :: Num a => [a] -> ([a] -> p) -> p
thousand [] c = c [1000]
thousand (x : xs) c = c $ (x * 1000) : xs

-- endregion
