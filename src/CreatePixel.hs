--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- CreatePixel
--

module CreatePixel ( Pixel(..)
                    , createPixel
                    ) where

import Text.Read            ( readMaybe )
import Control.Exception    ( throw )
import Errors               ( MyError(..) )

type PosX   = Int
type PosY   = Int
type ColorR = Int
type ColorG = Int
type ColorB = Int

data Pixel = Pixel (PosX, PosY) (ColorR, ColorG, ColorB)
instance Show Pixel where
    show (Pixel (x, y) (r, g, b)) = "(" ++ show x ++ ", " ++ show y ++ ") (" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"

data ParsingPixel = ParsingPixel (Maybe PosX, Maybe PosY) (Maybe ColorR, Maybe ColorG, Maybe ColorB)

data Token = Splitter Char | Number String
instance Show Token where
    show (Splitter c) = "Splitter: " ++ [c]
    show (Number n) = "Number: " ++ n

createPixel :: String -> Pixel
createPixel str = finalPixelCheck $ parsing $ reduceTokens 0 $ tokenize str

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('(':xs) = Splitter '(' : tokenize xs
tokenize (')':xs) = Splitter ')' : tokenize xs
tokenize (' ':xs) = Splitter ' ' : tokenize xs
tokenize (',':xs) = Splitter ',' : tokenize xs
tokenize (x:xs)   = Number [x]   : tokenize xs

reduceTokens :: [Token] -> [Token]
reduceTokens [] = []
reduceTokens (Number digit1 : Number digit2 : xs) = Number (digit1 ++ digit2) : reduceTokens xs
reduceTokens (v:xs) = v : reduceTokens xs
-- reduceTokens (Splitter '(':xs) = Splitter '(' : reduceTokens xs
-- reduceTokens (Splitter ')':xs) = Splitter ')' : reduceTokens xs
-- reduceTokens (Splitter ' ':xs) = Splitter ' ' : reduceTokens xs
-- reduceTokens (Splitter ',':xs) = Splitter ',' : reduceTokens xs
-- reduceTokens (Number digit1 : Number digit2 : xs) = Number (digit1 ++ digit2) : reduceTokens xs
-- reduceTokens (Number digit : xs) = Number digit : reduceTokens xs

parsing :: [Token] -> ParsingPixel
parsing [Splitter '(', Number x, Splitter ',', Number y, Splitter ')', Splitter ' ', Splitter '(', Number r, Splitter ',', Number g, Splitter ',', Number b, Splitter ')']
           = Pixel (readMaybe x, readMaybe y) (readMaybe r, readMaybe g, readMaybe b)
parsing _  = throw $ InputError "Error while parsing the file."

finalPixelCheck :: ParsingPixel -> Pixel
finalPixelCheck (ParsingPixel (Nothing, _      ) (_      , _      , _      )) = throw $ InputError "Error while parsing the file."
finalPixelCheck (ParsingPixel (_      , Nothing) (_      , _      , _      )) = throw $ InputError "Error while parsing the file."
finalPixelCheck (ParsingPixel (_      , _      ) (Nothing, _      , _      )) = throw $ InputError "Error while parsing the file."
finalPixelCheck (ParsingPixel (_      , _      ) (_      , Nothing, _      )) = throw $ InputError "Error while parsing the file."
finalPixelCheck (ParsingPixel (_      , _      ) (_      , _      , Nothing)) = throw $ InputError "Error while parsing the file."
finalPixelCheck (ParsingPixel (x, y) (r, g, b)) | r < 0 || r > 255 = throw $ InputError "Error while parsing the file: a color must be between 0 and 255."
                                                | g < 0 || g > 255 = throw $ InputError "Error while parsing the file: a color must be between 0 and 255."
                                                | b < 0 || b > 255 = throw $ InputError "Error while parsing the file: a color must be between 0 and 255."
                                                | otherwise = Pixel (x, y) (r, g, b)