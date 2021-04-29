--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- CompressorConf
--

module CompressorConf ( getCompressorConf, CompressorConf(..) ) where

-- import Control.Applicative  ( some )
import Parsing              ( Conf(..)
                            , NumberColors
                            , NumberLimit
                            )
import CreatePixel          ( Pixel
                            , createPixel
                            )

data CompressorConf = CompressorConf NumberColors NumberLimit [Pixel]
instance Show CompressorConf where
    show (CompressorConf n l pixels) = "Nb Colors: " ++ show n ++ ", Limit: " ++ show l ++ ", Pixels:\n" ++ show pixels

getCompressorConf :: Conf -> [String] -> CompressorConf
getCompressorConf (Conf nbColors nbLimit _) fileContent = CompressorConf nbColors nbLimit $ map createPixel fileContent
