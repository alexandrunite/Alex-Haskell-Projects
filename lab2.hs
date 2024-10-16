--sumaPatrate
sumaPatrate :: Integer -> Integer -> Integer
sumaPatrate x y = x^2 + y^2

parImpar :: Integer -> String
parImpar n = if even n
             then "este par"
             else "este impar"


factorial :: Integer -> Integer
factorial 0 = 1
factorial n =
    if n>0 
        then n*factorial(n-1)
        else error "nu merge"


maxim :: Integer -> Integer -> Integer
maxim x y =
    if (x>y)
        then x
        else y

maxim3 :: Integer -> Integer -> Integer -> Integer
maxim3 x y z = maxim x (maxim y z)

isGreaterThanDouble :: Integer -> Integer -> String
isGreaterThanDouble x y =
    if x > 2*y
        then "True"
        else "False"

maxim4 :: Integer -> Integer -> Integer -> Integer ->Integer
maxim4 x y z w =
    let
        u=maxim x y
        v=maxim z w
    in 
        maxim u v

maxList :: [Integer] -> Integer
maxList [a] = a
maxList (x:xs) = maxim x (maxList xs)

max2 :: [Int]->Int
max2[x] = xs
max2 (x:xs)
    (max2 xs) > x = max2 xs
    otherwise = x

poly :: Double->Double->Double->Double->Double
poly a b c x =
    let
        v= x*x
        in
            a*n+b*x+c
