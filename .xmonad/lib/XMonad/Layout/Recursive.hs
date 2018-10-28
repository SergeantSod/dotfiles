{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}
module XMonad.Layout.Recursive (Recursive(..)) where

import XMonad
import XMonad.Core
import XMonad.StackSet

-- A recursively dividing layout.
data Recursive a = Recursive {
                       adaptive::Bool,
                       ratio :: Rational,
                       delta :: Rational                       
                       }
                   deriving (Eq,Show,Read)

instance LayoutClass Recursive a where
    pureLayout spir rec stack = let windows = integrate stack
                                    r = ratio spir                                    
                                in part (adaptive spir) r rec windows
    pureMessage l = fmap resize . fromMessage
                    where r = ratio l
                          d = delta l
                          resize Expand = l{ratio=r + d}
                          resize Shrink = l{ratio=r - d}
    description spir = "Recursive "  -- ++ if (adaptive spir) then "Adaptive"
                                     --  else "Alternating"
    

part::Bool -> Rational -> Rectangle -> [a] -> [(a,Rectangle)]
part True = adaptivePart
part False = alternatingPart True

alternatingPart::Bool -> Rational -> Rectangle -> [a] -> [(a,Rectangle)]
alternatingPart _ _ r [x] = [(x,r)]
alternatingPart d ratio rec (x:xs) = 
    let (result,rest) = chop d ratio rec
    in (x,result):(alternatingPart (not d) ratio rest xs)

adaptivePart::Rational -> Rectangle -> [a] -> [(a,Rectangle)]
adaptivePart _ r [x] = [(x,r)]
adaptivePart ratio rec (x:xs) = 
    let dir = (rect_width rec) > (rect_height rec)
        (result,rest) = chop dir ratio rec
    in (x,result):(adaptivePart ratio rest xs)

gen_chop::  (Rectangle -> (Position,Dimension) ) ->               -- read
            (Rectangle -> Position -> Dimension -> Rectangle) ->   -- modify
            Rational -> Rectangle -> (Rectangle,Rectangle)
gen_chop read modify ratio rec = 
    let (pos,dim)   = read rec    
        new_dim     = round (ratio * (fromIntegral dim))
        rest_start  = pos + (fromIntegral new_dim)
        rest_dim    = dim - new_dim
        result      = modify rec pos new_dim
        rest        = modify rec rest_start rest_dim
    in (result,rest)

chop::Bool -> Rational -> Rectangle -> (Rectangle,Rectangle)
chop True = gen_chop x_read x_mod
chop False = gen_chop y_read y_mod

x_mod::Rectangle -> Position -> Dimension -> Rectangle
x_mod r x w = r{rect_x=x,rect_width=w}

x_read::Rectangle -> (Position,Dimension)
x_read rec =  (rect_x rec,rect_width rec)

y_mod::Rectangle -> Position -> Dimension -> Rectangle
y_mod r y h = r{rect_y=y,rect_height=h}

y_read::Rectangle -> (Position,Dimension)
y_read rec =  (rect_y rec,rect_height rec)


testRecursive = Recursive False (1/2)  (1/8)
testRectangle = Rectangle 0 0 2000 2000
testStack = Stack 4 [6,5] [1,2,3]

