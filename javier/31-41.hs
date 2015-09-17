import Debug.Trace

isPrime :: Int -> Bool
isPrime e | e < 1 = error "Error, negative integer"
isPrime 1 = True
isPrime 2 = True
isPrime n = not (sumOneDivs 2 n)
            where sumOneDivs x y 
                    | x == (y-1) = (y `mod` x == 0)
                    | otherwise  = (y `mod` x == 0) || sumOneDivs (x+1) y

myGCD :: Int -> Int -> Int
myGCD 0 0 = error "What are u doing dud?"
myGCD 0 x = x
myGCD x 0 = x
myGCD x y 
    | x < 0 =  myGCD (-x)   y
    | y < 0 =  myGCD   x  (-y)
    | otherwise = myGCD y ( x `mod` y)

coprime :: Int->Int->Bool
coprime x y =  myGCD x y == 1

totient :: Int->Int 
totient n = length [x | x <- [1..(n-1)] , coprime n x ]

-- Fucking slow
primeFactors :: Int-> [Int] 
primeFactors n | n <= 2 = []
primeFactors n =  [y | x <- [2..(n-1)], 
                       y <- replicate (multiplicity n x) x, 
                       isPrime x 
                  ]
                    where  multiplicity n a 
                            | n `mod` a == 0 = 1 + multiplicity (n `div` a) a 
                            | otherwise      = 0

prime_factors_mult :: Int -> [(Int,Int)]
prime_factors_mult n | n <= 2 = []
prime_factors_mult n =  [(x,y) | x <- [2..(n-1)], 
                           y <- [multiplicity n x], 
                           (multiplicity n x) > 0, 
                           isPrime x
                        ]
                    where  multiplicity n a 
                            | n `mod` a == 0 = 1 + multiplicity (n `div` a) a 
                            | otherwise      = 0

-- Estoy recorriendo la lista dos veces soy un cono
phi :: Int->Int
phi n = (foldl (*) 1 (map phiTransformation (prime_factors_mult n) ))
          where phiTransformation (a,b)  = (a-1) * a ^ (b-1)

-- Solucion de la pagina
phi' :: Int->Int
phi' n = product [ (a-1) * a ^ (b-1) | (a,b)<-prime_factors_mult n]
-- totient 0.14 secs, phi 0.07, phi' 0.06 secs LOL no lo esperaba

--version cono
primeR :: Int->Int->[Int]
primeR x y | x < 1 || y < 1 = error "Error! no pasaste primer grado"
primeR x y = [m | m <- [x..y] , isPrime m]

--Buena para rangos grandes, metodo de colegio, me dio ladilla. Pero hay que ir tachando jejeps
-- primeR' :: Int->Int->Int
-- primeR' x y | x < 1 || y < 1 = error "Error! no pasaste primer grado"
-- primeR' x y =  (zip [1..y] (repeat True))
--                 where deletEvery n ls = 
--                       getFirstPrime x = 

--Prueba de todos contra todos, metodo del indio jejeps
goldBach :: Int->(Int,Int)
goldBach n = head [ (x,y) | x <- (primeR 2 n), 
                            y <- (primeR 2 n), 
                            x + y == n 
                  ]

-- Por algun motivo no funciona para el 2, no me importa 
goldBachList a b = mapM_ putStrLn (map mahPrint (map goldBach ( filter even [a..b])))
                    where mahPrint (a,b) = (show (a+b)) ++" = " ++ show a ++ " + " ++ show b 
