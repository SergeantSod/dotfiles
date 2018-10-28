{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module XMonad.Layout.AdditionalGaps (gaps) where

import XMonad
import XMonad.Core
import XMonad.Layout.LayoutModifier

data Gaps i a = Gaps i  deriving (Read, Show)
instance (Integral i,Read i, Show i) => LayoutModifier (Gaps i) a where
    pureModifier _          _ _ [x] = ([x],Nothing)
    pureModifier (Gaps pix) _ _ r =
        let wins = map fst r
            recs = map snd r
            recs' = map (shrink pix) recs
        in ((zip wins recs'),Nothing)

shrink::(Integral i) => i -> Rectangle -> Rectangle
shrink pix r =
    let t1 = pix `div` 2
        x_start = (rect_x r) + (fromIntegral t1)
        x_len   = (rect_width r) - (fromIntegral pix)
        y_start = (rect_y r) + (fromIntegral t1)
        y_len   = (rect_height r) - (fromIntegral pix)
    in Rectangle {  rect_x = x_start,
                    rect_width = x_len,
                    rect_y = y_start,
                    rect_height = y_len}

gaps :: (Integral i,Read i)=> i -> l a -> ModifiedLayout (Gaps i) l a
gaps pix l = ModifiedLayout (Gaps pix) l
