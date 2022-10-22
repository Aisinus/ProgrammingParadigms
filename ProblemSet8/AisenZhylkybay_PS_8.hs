{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
import Data.List

-- Ex 1.a
isSingleton :: [a] -> Bool
isSingleton x 
    | length x == 1 = True
    | otherwise = False

--Ex 1.b
_insert :: Int -> [Int] -> [Int]
_insert n [] = [n]
_insert n (x:xs)
    | n < x = n:(x:xs)
    | otherwise = x : _insert n xs

-- Ex 1.c
separateBy :: a -> [a] -> [a]
separateBy _ [x] = [x]
separateBy s (x:xs) = x : (s : (separateBy s xs))

-- Ex 1.d
splitWhenNot :: (a -> Bool) -> [a] -> ([a], [a])
splitWhenNot _ [] = ([], [])
splitWhenNot f l = (takeWhile f l, dropWhile f l)


-- Ex 1.e
groupsSeparatedBy :: (a -> Bool) -> [a] -> [[a]]
groupsSeparatedBy _ [] = [[]]
groupsSeparatedBy f l = (takeWhile (not . f) l) : (groupsSeparatedBy f other ) where
    other = case (dropWhile (not . f) l) of
        [] -> []
        (x:xs) -> xs


-- Ex 1.f 
replicateWithPos :: [a] -> [a]
replicateWithPos xs = concatMap (uncurry replicate) (zip [1 .. ] xs)

-- Ex 2.a
lucas :: [Int]
lucas = f 2 1 where 
    f a b = a : f b (a + b)

-- Ex 2.b
approximationsOfRoot2 :: Double -> [Double]
approximationsOfRoot2 x = f x where 
    f x = x : f (x - (x / 2) + (1 / x))


main :: IO()
main = do
    print (isSingleton [1,2,3])
    print (_insert 3 [1,2,5,7])
    print (_insert 3 [0,1,1])
    print (take 5 (_insert 3 [1..]))
    print (separateBy ',' "hello")
    print (take 5 (separateBy 0 [1..]))
    print (splitWhenNot (/= ' ') "Hello, world!")
    print (take 10 (fst (splitWhenNot (< 100) [1..])))
    print (take 3 (groupsSeparatedBy (\n -> n `mod` 4 == 0) [1..]))
    print (replicateWithPos "Hello")
    print (take 10 lucas)
    print (take 5 (approximationsOfRoot2 1))