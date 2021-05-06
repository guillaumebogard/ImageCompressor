--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- CreatePixel
--

module FileParsing.CreatePixel where

import Text.Read            ( readMaybe )
import Control.Exception    ( throw )

import Errors               ( CompressorError(..) )
import FileParsing.PixelLexing
import FileParsing.Pixel
import Vector.Vector

type PosX     = Int
type PosY     = Int
type Pos2D    = (PosX, PosY)

data ParsingPixel = ParsingPixel (Maybe PosX, Maybe PosY) (Maybe ColorR, Maybe ColorG, Maybe ColorB)

createPixel :: String -> Pixel
createPixel str = finalPixelCheck $ parsing $ foldr tokenize [] str

-- make a package to include different steps of the createPixel process.

parsing :: [TOKEN] -> ParsingPixel
parsing [Splitter '(', Number x, Splitter ',', Number y, Splitter ')', Splitter ' ', Splitter '(', Number r, Splitter ',', Number g, Splitter ',', Number b, Splitter ')']
           = ParsingPixel (readMaybe x, readMaybe y) (readMaybe r, readMaybe g, readMaybe b)
parsing _  = throw FileParse

finalPixelCheck :: ParsingPixel -> Pixel
finalPixelCheck (ParsingPixel (Nothing, _      ) (_      , _      , _      )) = throw FileParse
finalPixelCheck (ParsingPixel (_      , Nothing) (_      , _      , _      )) = throw FileParse
finalPixelCheck (ParsingPixel (_      , _      ) (Nothing, _      , _      )) = throw FileParse
finalPixelCheck (ParsingPixel (_      , _      ) (_      , Nothing, _      )) = throw FileParse
finalPixelCheck (ParsingPixel (_      , _      ) (_      , _      , Nothing)) = throw FileParse
finalPixelCheck (ParsingPixel (Just x , Just y ) (Just r , Just g , Just b ))
        | isValidColor (Vector3 (r, g, b)) = Pixel (Vector2 (x, y)) $ Vector3 (r, g, b)
        | otherwise              = throw FileParseColorError

-- use isValidColor to check if a pixel whether be instantiated or not, if isValidColor false, throw FileParseColorError

isValidColor :: ColorRGB -> Bool
isValidColor (Vector3 (r, g, b))
        | r < 0 || r > 255 = False
        | g < 0 || g > 255 = False
        | b < 0 || b > 255 = False
        | otherwise        = True
