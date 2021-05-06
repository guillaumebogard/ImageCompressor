--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- Pixel
--

module FileParsing.Pixel where

import Vector.Vector

type ColorR = Int
type ColorG = Int
type ColorB = Int
type ColorRGB = Vector3 Int

data Pixel = Pixel (Vector2 Int) ColorRGB
instance Show Pixel where
    show (Pixel pos color) = show pos ++ ' ' : show color
instance Eq Pixel where
    (==) (Pixel _ (Vector3 (r1, g1, b1))) (Pixel _ (Vector3 (r2, g2, b2))) = r1 == r2 && g1 == g2 && b1 == b2
