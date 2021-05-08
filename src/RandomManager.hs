--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- RandomManager
--

module RandomManager ( makeNbRandomsUnique
                     , makeNbRandoms
                     ) where

import System.Random ( randomR
                     , RandomGen )
import Data.List     ( nub )

makeNbRandomsUnique :: (RandomGen a) => a -> Int -> Int -> [Int]
makeNbRandomsUnique seed nbLeft maxValue = makeNbRandomsUnique' seed nbLeft maxValue []

makeNbRandomsUnique' :: (RandomGen a) => a -> Int -> Int -> [Int] -> [Int]
makeNbRandomsUnique' seed nbLeft maxValue acc
        | nbLeft == length acc = acc
        | otherwise            = let (nacc, s) = makeNbRandoms seed (nbLeft - length acc) maxValue [] in makeNbRandomsUnique' s nbLeft maxValue $ nub $ acc ++ nacc

makeNbRandoms :: (RandomGen a) => a -> Int -> Int -> [Int] -> ([Int], a)
makeNbRandoms seed 0      _        acc = (acc, seed)
makeNbRandoms seed nbLeft maxValue acc = let (val, newSeed) = randomR (0, maxValue) seed in makeNbRandoms newSeed (nbLeft - 1) maxValue $ val : acc
