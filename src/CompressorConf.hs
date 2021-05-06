--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- CompressorConf
--

module CompressorConf ( getCompressorConf, CompressorConf(..) ) where

import ArgumentParsing.Parsing              ( Conf(..)
                            , NumberColors
                            , NumberLimit
                            )
import FileParsing.CreatePixel
import FileParsing.Pixel

data CompressorConf = CompressorConf NumberColors NumberLimit [Pixel]

getCompressorConf :: Conf -> [String] -> CompressorConf
getCompressorConf (Conf nbColors nbLimit _) fileContent = CompressorConf nbColors nbLimit $ map createPixel fileContent
