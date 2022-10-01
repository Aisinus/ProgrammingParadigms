--1.a
binaryToDecimal :: [Int] -> Int
binaryToDecimal [] = 0
binaryToDecimal (x:xs) = x * (2 ^ length xs) + binaryToDecimal xs

--1.b
countZeros :: [Int] -> Int
countZeros (x:xs)
  | x<=0 = countZeros xs
  |otherwise = f (xs) where
     f[] = 0
     f (x:xs) = (0 ^ x) + f xs

--1.c
encodeWithLengths :: [Int] -> [Int]
encodeWithLengths (x:xs)
    |x==0 = encodeWithLengths xs
    |otherwise = helper (xs, 1, 1)
    
helper :: ([Int], Int, Int) -> [Int]
helper ([], current, count) = [count]
helper (x:xs, current, count)
  | x == current = helper (xs, current, count+1)
  | otherwise = count : helper (xs, x, 1)
        

--1.d
binaryOdd :: [Int] -> Bool
binaryOdd [] = False
binaryOdd l = f (reverse l) where
  f(x:xs)
    |x==0 = False
    |otherwise = True


--1.e
decrement :: [Int] -> [Int]
decrement[x] = [0]
decrement l = f (reverse (decrement_helper (reverse(l),0))) where
   f (x:xs)
     |x==0 = f xs
     |otherwise = x:xs
decrement_helper :: ([Int], Int) -> [Int]
decrement_helper ([],check) = []
decrement_helper (x:xs, check)
  |check == 1 = x : decrement_helper(xs,check)
  |x==0 = 1 : decrement_helper(xs,0)
  |x==1 = 0 : decrement_helper(xs,1)



--1.f
propagate :: (Bool, [Int]) -> [(Bool, Int)]
propagate (bool, []) = []
propagate (bool, (x:xs)) = (bool, x) : propagate (bool, xs)


--2.a
alternatingSum :: [Int] -> Int
alternatingSum [] = 0
alternatingSum (x:xs) = x - alternatingSum xs

--2.b
-- = alternatingSum ([1,2,3,4,5])
-- = x-alternatingSum xs
--    where x = 1, xs = [2,3,4,5]
-- = 1-(x-alternatingSum xs)
--    where x = 2, xs = [3,4,5]
-- = 1-(2-(x-alternatingSum xs))
--    where x = 3, xs = [4,5]
-- = 1-(2-(3-(x-alternatingSum xs)))
--    where x = 4, xs = [5]
-- = 1-(2-(3-(4-(x-alternatingSum xs))))
--    where x = 5, xs = []
-- = 1-(2-(3-(4-(5-0))))
-- = 3


--3

data Radians = Radians Double
data Degrees = Degrees Double

pi' :: Double
pi' = 3.14159

toDegrees :: Radians -> Degrees
toDegrees (Radians x) = Degrees (x * 180/ pi')

fromDegrees :: Degrees -> Radians
fromDegrees (Degrees x) = Radians (x*pi'/180)


main :: IO ()
main = print (pi')
