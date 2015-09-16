import Debug.Trace
-- Implementado por un cono
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

pack :: (Eq a, Show a) => [a] -> [[a]]
pack []  = []
pack [x] = [[x]]
pack (x:xs)  = compressOnLeft [] [x] xs False
            where compressOnLeft res [] _ _      = res
                  compressOnLeft res acc l False
                    | head acc == head l = trace ("Hola my lista es" ++ show l) (compressOnLeft res ([head l]++acc) (tail l) False )
                    | otherwise          = (compressOnLeft res acc l True )
                  compressOnLeft res acc l True
                    | l == []   = compressOnLeft (res ++ [acc]) [] [] False           -- Cuello de botella en la ++
                    | otherwise = trace ("Chao my lista es" ++ show l) compressOnLeft (res ++ [acc]) [head l] (tail l) False

-- Ultima recursion explota dunno what todo
-- explosion = pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a',  'a', 'd', 'e', 'e', 'e', 'e']
-- main = print explosion