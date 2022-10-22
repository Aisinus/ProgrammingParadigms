{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
module Main where
import Data.Char (toUpper)

-- Ex 1
guess :: (a -> Bool) -> (String -> IO a) -> IO a
guess p g = do
    s <- getLine
    x <- g s 
    case p x of
        True -> return x
        False -> guess p g

checkFunction :: a -> Bool
checkFunction x = False

checkFunction2 :: String -> IO ()
checkFunction2 x= do putStrLn x
--- As you can see from this example, the guess function will always return the value from getLine, whether recursion is running or not. Therefore, the return type of the function will be IO

-- Ex 2
echo :: IO ()
echo = do 
    line <- getLine
    putStrLn (echoHelper line)
    echo
echoHelper :: [Char] -> String
echoHelper [] = []
echoHelper (x:xs) = toUpper x : echoHelper xs

--Ex 3.a
foreverIO :: IO a -> IO b
foreverIO g = do
    g
    foreverIO g

--Ex 3.b
whenIO :: Bool -> IO () -> IO ()
whenIO c f = case c of
    True -> do f
    False -> putStrLn("Condition is not satisfied")


--Ex 3.c
maybeIO :: Maybe (IO a) -> IO (Maybe a)
maybeIO (Just value) = do 
    x <- value
    return (Just x)
maybeIO (Nothing) = return Nothing

--asd
checkMaybeIO :: a -> (a -> IO b) -> Maybe (IO b)
checkMaybeIO l f = Nothing
--checkMaybeIO l f = Just (f l)


--Ex 3.d
sequenceMaybeIO :: [IO (Maybe a)] -> IO [a]
sequenceMaybeIO [] = return []
sequenceMaybeIO (x:xs) = do
    a <- x
    as <- (sequenceMaybeIO xs)
    case a of
        Just val -> return(val:as)
        Nothing -> return(as)

--Ex 3.e
whileJustIO :: (a -> IO (Maybe a)) -> a -> IO ()
whileJustIO f g = do
    x <- f g
    case x of
        Just val -> whileJustIO f val 
        Nothing -> putStrLn("Nothing")


--Ex 3.f
forStateIO_ :: s -> [a] -> (a -> s -> IO s) -> IO s
forStateIO_ s_ [] f = return s_
forStateIO_ s_ (x:xs) f = do
    s <- f x s_
    forStateIO_ s xs f

verboseCons :: Int -> [Int] -> IO [Int]
verboseCons x xs = do
    putStrLn ("prepending " ++ show x ++ " to " ++ show xs)
    return (x:xs)


--Ex 4
iforIO_ :: [a] -> (Int -> a -> IO ()) -> IO ()
iforIO_ [] f = return ()
iforIO_ list f = iforIO_helper 0 list f

iforIO_helper :: Int -> [a] -> (Int -> a -> IO ()) -> IO ()
iforIO_helper i [] f = return()
iforIO_helper i (x:xs) f = do
    f i x
    iforIO_helper (i+1) xs f


main :: IO()
main = do
    iforIO_ [1, 2] (\i n ->
            iforIO_ "ab" (\j c ->
                print ((i, j), replicate n c)))
    -- let check = [maybeIO (checkMaybeIO "je;;p" putStrLn), maybeIO (checkMaybeIO "je;;p" putStrLn), maybeIO (checkMaybeIO "je;;p" putStrLn)]
    -- sequenceMaybeIO check
    -- x <- maybeIO (checkMaybeIO "je;;p" putStrLn)
    -- case x of 
    --     Just value ->
    --         putStrLn("Value is here")
    --     Nothing ->
    --         putStrLn("Got Nothing") 
    x <- forStateIO_ [0] [1,2,3] verboseCons
    putStrLn("Ended")
    
        
    
