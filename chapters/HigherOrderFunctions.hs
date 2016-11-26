-- Higher Order function
-- http://learnyouahaskell.com/higher-order-functions

-- Curried functions
-- =================

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- Notice here the compare parameter is not explecitly
-- defined in the function body

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

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

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