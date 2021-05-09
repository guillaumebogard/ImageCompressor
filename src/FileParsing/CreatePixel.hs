--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- CreatePixel
--

module FileParsing.CreatePixel  ( createPixel ) where

import Text.Read                ( readMaybe )
import Control.Exception        ( throw )

import Errors                   ( CompressorError(..) )
import FileParsing.PixelLexing  ( TOKEN(..)
                                , tokenize )
import FileParsing.Pixel        ( ColorRGB
                                , Pixel(..)
                                , ColorB
                                , ColorG
                                , ColorR )
import Vector.Vector            ( Vector3(Vector3)
                                , Vector2(Vector2) )

data ParsingPixel = ParsingPixel (Maybe Int, Maybe Int) (Maybe ColorR, Maybe ColorG, Maybe ColorB)

createPixel :: String -> Pixel
createPixel str = finalPixelCheck $ parsing $ tokenize str

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
    | isValidColor (Vector3 r g b) = Pixel (Vector2 x y) $ Vector3 r g b
    | otherwise                    = throw FileParseColorError

isValidColor :: ColorRGB -> Bool
isValidColor (Vector3 r g b)
    | r < 0 || r > 255 = False
    | g < 0 || g > 255 = False
    | b < 0 || b > 255 = False
    | otherwise        = True
