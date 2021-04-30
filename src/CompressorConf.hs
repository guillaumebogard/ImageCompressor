--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- CompressorConf
--

module CompressorConf ( getCompressorConf, CompressorConf(..) ) where

import Parsing              ( Conf(..)
                            , NumberColors
                            , NumberLimit
                            )
import CreatePixel          ( Pixel
                            , createPixel
                            )

data CompressorConf = CompressorConf NumberColors NumberLimit [Pixel]

getCompressorConf :: Conf -> [String] -> CompressorConf
getCompressorConf (Conf nbColors nbLimit _) fileContent = CompressorConf nbColors nbLimit $ map createPixel fileContent
