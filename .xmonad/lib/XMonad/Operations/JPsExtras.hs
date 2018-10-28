module XMonad.Operations.JPsExtras(focusSecondTop,moveToSecondTop) where

import XMonad.StackSet

focusSecondTop::(Eq s, Eq a, Eq i) => StackSet i l a s sd -> StackSet i l a s sd
focusSecondTop s =
    let maybeSecond = maybe Nothing secondTop (maybeStack s)
    in maybe s (\st -> focusWindow st s) maybeSecond

secondTop::Stack a -> Maybe a
secondTop x = case integrate x of
                    _:(target:_)    -> Just target
                    _               -> Nothing
maybeStack::StackSet i l a s sd -> Maybe (Stack a)
maybeStack = stack . workspace . current

moveToSecondTop::StackSet i l a s sd -> StackSet i l a s sd
moveToSecondTop =
    let f::Stack a -> Stack a
        f s = let foc = focus s
                  l = (reverse . up $ s) ++ (down s)
               in case l of 
                    (x:xs) -> Stack {
                                    focus = foc,
                                    up = [x],
                                    down = xs
                                }
                    _ -> s
    in modify' f
    
