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

data Compress a = Multiple  Int a | Single a deriving (Show)

encodeModified :: (Eq a,Show a) => [a] -> [Compress a]
encodeModified x = map convert (encode x)
                  where convert (1,e) = (Single e)
                        convert (n,e) = (Multiple n e)

decodeModified :: (Eq a,Show a) => [Compress a] -> [a]
decodeModified x = foldl (++) [] (map deconvert x)
                    where deconvert (Single e)     = [e]
                          deconvert (Multiple n e) = replicate n e

dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = (x:x:dupli xs) 

repli :: [a] -> Int -> [a]
repli [] _ = []
repli arg n = countDown n arg
                  where countDown _   []     = []
                        countDown 0 (x:xs)   = countDown n xs
                        countDown n l@(x:xs) = (x:countDown (n-1) l )

repli' :: [a] -> Int -> [a]
repli' [] _ = []
repli' (a:as) n = (replicate n a) ++ (repli' as n)

dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery n a = takeNonZeroes a n
                where takeNonZeroes [] _     = []
                      takeNonZeroes (x:xs) 1 = takeNonZeroes xs n
                      takeNonZeroes (x:xs) i = (x : takeNonZeroes xs (i-1))

split :: [a] -> Int -> ([a],[a])
split l n = fillLeft ([],[]) n l
              where fillLeft (l1,_) 0  l2 = (l1,l2)
                    fillLeft (l1,_) i  (x:xs) = fillLeft (l1++[x],[]) (i-1) xs -- jeje concat

-- Dos recorridos de lista (Way better)
-- Uno implementa take y el otro drop
split' :: [a] -> Int -> ([a],[a])
split' l n =  (expandTill l n , takeRest l n) 
            where expandTill (x:xs) 0 = []         
                  expandTill (x:xs) n = (x: expandTill xs (n-1))
                  takeRest    l     0   =  l
                  takeRest   (x:xs) n = takeRest xs (n-1)

-- Un solo recorrido de lista
slice :: [a] -> Int -> Int -> [a]
slice = dropUntil
          where dropUntil l      0 e = keepUntil  l e
                dropUntil (x:xs) 1 e = x:keepUntil xs e
                dropUntil (x:xs) b e = dropUntil xs (b-1) (e-1)
                keepUntil (x:xs)   1 = []
                keepUntil (x:xs)   e = x:keepUntil xs (e-1)

-- Piden usar lengh y ++ 
rotate :: [a] -> Int -> [a]
rotate l n  
  | n == 0    = l
  | n  > 0    = concatTillZ l [] n
  | otherwise = concatTillZ l [] ((length l) + n) 
      where concatTillZ (x:xs) t 0 = x:(xs++reverse t)
            concatTillZ (x:xs) t n = concatTillZ xs ([x]++t) (n-1) -- agregando al reves

removeAt :: Int -> [a] -> (a,[a])
removeAt 0 l = error "Undefined index"
removeAt n l = keepLooking n [] l
                where keepLooking 1 acc (x:xs) = ( x , acc ++ xs)
                      keepLooking n acc (x:xs) = keepLooking (n-1) (acc ++ [x]) xs --Ineficiente