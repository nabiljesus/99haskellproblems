myLast :: [a] -> a
myLast []     = error "Lista vacia"
myLast [a]    = a
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast []     = error "Listas vacias"
myButLast [a,b]  = a
myButLast (x:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt  _ n | n <= 0 = error "No such index"
elementAt (x:_ ) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)

myLength :: [a] -> Int
myLength []  = 0
myLength (_:xs) = 1 + myLength xs

-- Version cono
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]
