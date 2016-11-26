-- Higher Order function
-- http://learnyouahaskell.com/higher-order-functions

-- Curried functions
-- =================

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- Notice here the compare parameter is not explecitly defined in the function 
-- body

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

-- Partial application of infix functions

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphaNum :: Char -> Bool
isUpperAlphaNum = (`elem` ['A'..'Z'])


-- Some higher-orderism is in order
-- ================================

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Leverage partial application to flip params

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

-- map' :: (a -> b) -> [a] -> [b]
-- map' _ [] = []
-- map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x       = x : filter' p xs
  | otherwise = filter' p xs

-- Lazyness and filtering in action
-- let p x = x `mod` 3829 == 0 in head (filter p [100000,99999..])

-- TakeWhile
-- sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n  = n:chain (n `div` 2)
  | odd n   = n:chain (n*3 + 1)

-- Lambdas
-- =======

numLongChains :: Int
numLongChains = length ( filter (\xs -> length xs > 15) ( map chain [1..100]))

-- Used with map
-- map (\(a,b) -> a + b) [(1,2),(3,4)]

-- Are equivalents
-- AddThree = \x -> \y -> \z -> x + y + z
-- AddThree x y z = x + y + z

-- More readable way to define flip using lambda
-- flip' :: (a, b, c) -> b -> a -> c
-- flip' f = \x y -> f y x

-- Fold
-- ====

sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (\acc x -> acc + x) 0 xs
sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' x = foldl (\acc y -> if x == y then True else acc) False

-- Can also be done via foldl however it's less expensive to do it with foldr
-- : operator is less expensive than ++
map' :: ( a -> b ) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- Note: foldl is traversing the list in the left dirction from right to left
-- This function can't be used on infinite list

-- foldl1 and foldr1 use the first and last value respectivly as init value for 
-- the accumulator
head' :: [a] -> a
head' = foldr1 (\x _ -> x)   