{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

type Name = String
data Grade = A | B | C | D
data Student = Student Name Grade

data Result a 
    = Success a
    | Failure String
    

dup ::  (t1 -> t1 -> t2) -> t1 -> t2
dup f x = f x x

dip :: (t -> t -> t) -> t ->t -> t
dip f x = f (f x x)

twice :: (t -> t) -> t -> t
twice f x = f (f x)



-- 1.a
-- dip (+) 1 2
-- dip :: (t -> t -> t) -> t -> t -> t
-- (+) :: (Int -> Int -> Int)
-- 1 :: Int
-- 2 :: Int
-- Int -> Int -> Int = t -> t -> t
-- t = Int
-- dip (+) 1 :: (Int -> Int)
-- dip (+) 1 2 :: Int

-- 1.b
-- dup (dip (+)) 1
-- dup ::  (t1 -> t1 -> t2) -> t1 -> t2
-- dip :: (t -> t -> t) -> t -> t -> t
-- dip (+) :: Int->Int->Int
-- 1 :: Int
-- t1 -> t1 -> t2 = Int->Int->Int
-- t1 = Int
-- t2 = Int
-- dup (dip (+)) :: Int -> Int
-- dup (dip (+)) 1 :: Int

-- 1.c
-- twice dip
-- twice :: (t->t)->t->t
-- dip :: (t1->t1->t1)->t1->t1->t1
-- (t->t) = (t1->t1->t1)->t1->t1->t1 
-- Here we have not enough arguments, hence that I can't infer the type
-- However, if we assume that the operator +, -, * and the argument is Int, then the function will return the type Int


-- 1.d
-- dip dip
-- dip1 = (t->t->t)->t->t->t
-- dip2 = (t2->t2->t2)->t2->t2->t2
-- (t->t->t) = (t2->t2->t2)->t2->t2->t2
-- t2->t2->t2 = t
-- t2 = t
-- We have t2 = t2->t2->t2, indeed this means that t2 is an infinite type 
-- Haskell does not allow infinite types, so this expression leads to a type error

-- 1.e
-- twice twice twice
-- twice1 :: (t->t)->t->t
-- twice2 :: (t2->t2)->t2->t2
-- twice3 :: (t3->t3)->t3->t3
-- (t->t) = (t2->t2)->t2->t2
-- (t2->t2) = (t3->t3)->t3->t3
-- (t->t) = ((t3->t3)->t3->t3)->t2->t2
-- Here we have not enough arguments, hence that I can't infer the type
-- However, if we assume that the operator +, -, * and the argument is Int, then the function will return the type Int


-- 1.c
-- dup twice
-- dup :: (t1 -> t1 -> t2) -> t1 -> t2
-- twice :: (t->t)->t->t
-- (t1 -> t1 -> t2) = (t->t)->t->t
-- t1 = t->t
-- t1 = t
-- t2 = t
-- We have t1 = t1->t1, indeed this means that t1 is an infinite type 
-- Haskell does not allow infinite types, so this expression leads to a type error

-- 2
studentsWithA :: [Student] -> [Name]
studentsWithA [] = []
studentsWithA (Student name A : students) = name : studentsWithA students
studentsWithA (x:xs) = studentsWithA xs

-- 3.a
whileSuccess :: (a -> Result a) -> a -> a
whileSuccess f x = g (f x)
    where 
        g (Success y) = whileSuccess f y
        g (Failure y) = x

f n | n > 100 = Failure "input is too large"
    | otherwise = Success (2 * n)


-- 3.b
applyResult :: Result (a -> b) -> Result a -> Result b
applyResult (Success x) (Success y) = Success (x y)
applyResult (Failure x) _ = Failure (x)
applyResult _ (Failure y) = Failure (y)



example1 = f (applyResult (Success length) (Success [1, 2, 3]))
        where
          f (Success x) = x 
          --f (Failure x) = x
          --If apply Failure uncomment upper code, else comment them
          
          
-- 3.c
fromResult :: (a -> b) -> (String -> b) -> Result a -> b
fromResult f g (Failure a) = g a
fromResult f g (Success a) = f a


-- 3.d
combineResultsWith :: (a -> b -> c) -> Result a -> Result b -> Result c
combineResultsWith f (Success x) (Success y) = Success (f x y) 
combineResultsWith f (Failure x) _ = Failure x
combineResultsWith f _ (Failure y) = Failure y

example2 = f (combineResultsWith (+) (Failure "x is undefined") (Failure "crash"))
        where
          --f (Success x) = x 
          --If apply Success uncomment upper code, else comment them
          f (Failure x) = x
          --If apply Failure uncomment upper code, else comment them
          
main::IO()          
main = print(example2)

