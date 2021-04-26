--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- CompressorConf
--

module CompressorConf (getCompressorConf) where

import Control.Applicative ( some )
import Parsing ( Conf(..)
               , NumberColors
               , NumberLimit
               )
import CreatePixel

data CompressorConf = CompressorConf NumberColors NumberLimit [Pixel]
instance Show CompressorConf where
    show (CompressorConf n l pixels) = "Nb Colors: " ++ show n ++ ", Limit: " ++ show l ++ ", Pixels:\n" ++ show pixels

getCompressorConf :: Conf -> [String] -> CompressorConf
getCompressorConf (Conf nbColors nbLimit stringFilepath) fileContent = CompressorConf 0 0 $ getPixels fileContent

getPixels :: [String] -> [Pixel]
getPixels = map createPixel
-- getPixels [] = []
-- getPixels (l:lx) = createPixel l : getPixels lx

-- Pixel ::= Pos ' ' Color
-- Pos   ::= '(' int ',' int ')'
-- Color ::= '(' short ',' short ')'

-- createPixel :: String -> Pixel
-- createPixel _ = Pixel (0, 0) (0, 0, 0)
