--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- PixelLexing
--

module FileParsing.PixelParsing ( parsing
                                , ParsingPixel(ParsingPixel)
                                ) where

import Text.Read                ( readMaybe )
import Control.Exception        ( throw )

import Errors                   ( CompressorError(FileParse) )
import FileParsing.PixelLexing  ( TOKEN(..) )
import FileParsing.Pixel        ( ColorR
                                , ColorG
                                , ColorB )

data ParsingPixel = ParsingPixel (Maybe Int, Maybe Int) (Maybe ColorR, Maybe ColorG, Maybe ColorB)

parsing :: [TOKEN] -> ParsingPixel
parsing [Splitter '(', Number x, Splitter ',', Number y, Splitter ')', Splitter ' ', Splitter '(', Number r, Splitter ',', Number g, Splitter ',', Number b, Splitter ')']
           = ParsingPixel (readMaybe x, readMaybe y) (readMaybe r, readMaybe g, readMaybe b)
parsing _  = throw FileParse