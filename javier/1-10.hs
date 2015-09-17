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

-- Muuuuuu
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Implementado por humanos
myReverse' :: [a] -> [a]
myReverse' [] = []
myReverse' l = emptyList [] l
                where   emptyList _    []    = []
                        emptyList [] (x:xs)  = [x] ++ emptyList [] xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome []  = True
isPalindrome l   = firstEqs l (myReverse' l)
                    where firstEqs (x:xs) (y:ys) = (x == y) && firstEqs xs ys
                          firstEqs [] []         = True

data NestedList a = Elem a | List [NestedList a] deriving(Show)

flatten :: NestedList a -> [a]
flatten (Elem x)   = [x]
flatten (List nl)  =  fill [] nl
                       where fill ls []       = ls
                             fill ls [x]      = ls ++ flatten x
                             fill ls (x:xs)   = ls ++ flatten x ++ fill [] xs

compress :: Eq a => [a] -> [a]
compress []  = []
compress [x] = [x]
compress (x:(y:ys)) 
    |x == y = compress (x:ys)
    |x /= y = [x] ++ compress (y:ys)

-- Usando unas concatenaciones medio malas meh
pack :: (Eq a, Show a) => [a] -> [[a]]
pack []  = []
pack [x] = [[x]]
pack (x:xs)  = compressOnLeft [] [x] xs False
            where compressOnLeft res []  []  _  = res 
                  compressOnLeft res acc []  _  = res ++ [acc]
                  compressOnLeft res acc l False
                    | head acc == head l = compressOnLeft res ([head l]++acc) (tail l) False 
                    | otherwise          = compressOnLeft res acc l True 
                  compressOnLeft res acc l True
                    | l == []   = compressOnLeft (res ++ [acc]) [] [] False           -- Cuello de botella en la ++
                    | otherwise =  compressOnLeft (res ++ [acc]) [head l] (tail l) False

encode :: (Eq a, Show a) => [a] -> [(Int,a)]
encode [] = []
encode x = map makePair (pack x)
            where makePair ls = (length ls, head ls)