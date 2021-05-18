--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- Pixel
--

module FileParsing.Pixel  ( ColorRGB
                          , Pixel(..)
                          , ColorR
                          , ColorB
                          , ColorG
                          ) where

import Vector             ( Vector2, Vector3(..) )

-- newtype ColorR = ColorR ColorUnit
-- newtype ColorB = ColorB ColorUnit
-- newtype ColorG = ColorG ColorUnit -- and you derive to get sub and etc
type ColorUnit = Int
type ColorR    = ColorUnit
type ColorG    = ColorUnit
type ColorB    = ColorUnit
type ColorRGB  = Vector3 ColorUnit

type PixelPos  = Vector2 Int

data Pixel = Pixel PixelPos ColorRGB -- make a factory that check the color (isValidColor :: ColorRGB -> Bool) (and throw)
instance Show Pixel where
    show (Pixel pos color) = show pos ++ ' ' : show color
instance Eq Pixel where
    (==) (Pixel _ (Vector3 r1 g1 b1)) (Pixel _ (Vector3 r2 g2 b2)) = r1 == r2 && g1 == g2 && b1 == b2
