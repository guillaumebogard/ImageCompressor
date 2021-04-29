--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- CreatePixel
--

module CreatePixel ( Pixel(..)
                    , ColorRGB
                    , createPixel
                    ) where

import Text.Read            ( readMaybe )
import Control.Exception    ( throw )
import Errors               ( MyError(..) )

type PosX     = Int
type PosY     = Int
type Pos2D    = (PosX, PosY)

type ColorR   = Int
type ColorG   = Int
type ColorB   = Int
type ColorRGB = (ColorR, ColorG, ColorB)

data Pixel = Pixel Pos2D ColorRGB
instance Show Pixel where
    show (Pixel pos color) = show pos ++ ' ' : show color
instance Eq Pixel where
    (==) (Pixel (xa, ya) (ra, ga, ba)) (Pixel (xb, yb) (rb, gb, bb)) = ra == rb && ga == gb && ba == bb && xa == xb && ya == yb

data ParsingPixel = ParsingPixel (Maybe PosX, Maybe PosY) (Maybe ColorR, Maybe ColorG, Maybe ColorB)

data Token = Splitter Char | Number String
instance Show Token where
    show (Splitter c) = "Splitter: '" ++ [c] ++ "'"
    show (Number n)   = "Number: " ++ n

createPixel :: String -> Pixel
createPixel str = finalPixelCheck $ parsing $ foldr tokenize [] str

tokenize :: Char -> [Token] -> [Token]
tokenize '(' t             = Splitter '('   : t
tokenize ')' t             = Splitter ')'   : t
tokenize ' ' t             = Splitter ' '   : t
tokenize ',' t             = Splitter ','   : t
tokenize x   (Number a:ts) = Number (x : a) : ts
tokenize x   t             = Number [x]     : t

parsing :: [Token] -> ParsingPixel
parsing [Splitter '(', Number x, Splitter ',', Number y, Splitter ')', Splitter ' ', Splitter '(', Number r, Splitter ',', Number g, Splitter ',', Number b, Splitter ')']
           = ParsingPixel (readMaybe x, readMaybe y) (readMaybe r, readMaybe g, readMaybe b)
parsing a  = throw $ ArgumentError $ "There was an Error: " ++ show a

finalPixelCheck :: ParsingPixel -> Pixel
finalPixelCheck (ParsingPixel (Nothing, _      ) (_      , _      , _      )) = throw FileParse
finalPixelCheck (ParsingPixel (_      , Nothing) (_      , _      , _      )) = throw FileParse
finalPixelCheck (ParsingPixel (_      , _      ) (Nothing, _      , _      )) = throw FileParse
finalPixelCheck (ParsingPixel (_      , _      ) (_      , Nothing, _      )) = throw FileParse
finalPixelCheck (ParsingPixel (_      , _      ) (_      , _      , Nothing)) = throw FileParse
finalPixelCheck (ParsingPixel (Just x , Just y ) (Just r , Just g , Just b )) | r < 0 || r > 255 = throw FileParseColorError
                                                                              | g < 0 || g > 255 = throw FileParseColorError
                                                                              | b < 0 || b > 255 = throw FileParseColorError
                                                                              | otherwise        = Pixel (x, y) (r, g, b)