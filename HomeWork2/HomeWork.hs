{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

import Data.List
import CodeWorld

--Student Zhylkybay Aisen
--Ex 1

data Line a = Line [a] a [a]
    deriving(Show)

integers :: Line Integer
integers = Line [-1, -2..] 0 [1, 2..]

--Ex 1.1
cutLine :: Int -> Line a -> Line a
cutLine num (Line l focus r) = Line (take num l) focus (take num r)


--Ex 1.2
genLine :: (a -> Maybe a) -> a -> (a -> Maybe a) -> Line a
genLine f x g = Line (helper f x) x (helper g x) where
    helper a b = 
        case a b of
            Just val -> val : (helper a val)
            Nothing -> []

applyIf :: (a -> Bool) -> (a -> b) -> a -> Maybe b
applyIf p f x
  | p x       = Just (f x)
  | otherwise = Nothing



--Ex 1.3
mapLine :: (a -> b) -> Line a -> Line b
mapLine fun (Line l focus r) = Line (map fun l) (fun focus) (map fun r)

--Ex 1.4
zipLines :: Line a -> Line b -> Line (a, b)
zipLines (Line l f r) (Line l1 f1 r1) = Line (zip l l1) (f, f1) (zip r r1)

zipLinesWith :: (a -> b -> c) -> Line a -> Line b -> Line c
zipLinesWith fun (Line l f r) (Line l1 f1 r1) = Line (zipWith fun l l1) (fun f f1) (zipWith fun r r1)

--Ex 1.5
data Cell = Alive | Dead
    deriving (Show)

rule30 :: Line Cell -> Cell
rule30 (Line (l:_) focus (r:__)) = 
    case (l, focus, r) of
        (Alive, Dead, Dead) -> Alive
        (Dead, Alive, Alive) -> Alive
        (Dead, Alive, Dead) -> Alive
        (Dead, Dead, Alive) -> Alive
        otherwise -> Dead


--Ex 1.6
shiftLeft :: Line a -> Maybe (Line a)
shiftLeft (Line [] focus _) = Nothing
shiftLeft (Line (l:ls) focus r) = Just (Line ls l (focus:r)) 
 
shiftRight :: Line a -> Maybe (Line a)
shiftRight (Line _ focus []) = Nothing
shiftRight (Line l focus (r:rs)) = Just (Line (focus:l) r rs) 

--Ex 1.7
lineShifts :: Line a -> Line (Line a)
lineShifts (Line l focus r) = Line (helper shiftLeft (Line l focus r)) (Line l focus r) (helper shiftRight (Line l focus r)) where
    helper fun inp = 
        case fun inp of
            Just val -> val : helper fun val
            Nothing -> []

applyRule30 :: Line Cell -> Line Cell
applyRule30 line = mapLine rule30 (lineShifts line)

--Ex 1.8
sampleLine :: Line Picture
sampleLine = Line [a,b,c,d,e,f,g] c [g,d,b,c,a,f]
  where
    a = colored red (solidCircle 0.5)
    b = colored green (solidCircle 0.5)
    c = colored blue (solidCircle 0.5)
    d = colored yellow (solidCircle 0.5)
    e = colored purple (solidCircle 0.5)
    f = colored brown (solidCircle 0.5)
    g = colored pink (solidCircle 0.5)
    

renderLine :: Line Picture -> Picture
renderLine (Line l focus r) = (translated (-1) 0 (render (-1) l)) <> focus <> (translated 1 0 (render 1 r)) where
  render _ [] = blank
  render step (x:xs) = x <> (translated step 0 (render step xs))

renderRule30 :: Int -> Line Cell -> Picture
renderRule30

main :: IO()
main = do
    print(cutLine 3 integers)
    print(genLine (applyIf (> -3) (subtract 1)) 0 (applyIf (< 3) (+1)))
    print(genLine (\_ -> Nothing) 1 (\_ -> Nothing))
    print(cutLine 3 (genLine Just 0 Just))
    print(mapLine (^2) (Line [-1, -2, -3] 0 [1, 2, 3]))
    print(zipLines ((Line [-1, -2, -3] 0 [1, 2, 3])) ((Line [-1, -2, -3] 0 [1, 2, 3])))
    print(zipLinesWith (*) ((Line [-1, -2, -3] 0 [1, 2, 3])) ((Line [-1, -2, -3] 0 [1, 2, 3])))
    print(rule30 (Line [Dead, Alive, Alive] Alive [Alive, Alive, Alive]))
    print(rule30 (Line [Alive, Alive, Alive] Alive [Alive, Alive, Alive]))
    print(shiftRight (Line [0,1,1] 1 [1,1,1]))
    print(shiftLeft (Line [0,1,1] 1 [1,1,1]))
    print(lineShifts (Line [2,1] 3 [4,5]))
    print(cutLine 4 $ applyRule30 (Line ([Dead, Alive, Alive] ++ repeat Dead) Alive ([Alive, Alive, Alive] ++ repeat Dead)))
    drawingOf (renderLine sampleLine)
