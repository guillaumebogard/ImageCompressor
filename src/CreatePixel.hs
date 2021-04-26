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
    show (Splitter c) = "Splitter: '" ++ [c] ++ "'"
    show (Number n)   = "Number: " ++ n

createPixel :: String -> Pixel
createPixel str = finalPixelCheck $ parsing $ reduceTokens $ tokenize str

tokenize :: String -> [Token]
tokenize []       = []
tokenize ('(':xs) = Splitter '(' : tokenize xs
tokenize (')':xs) = Splitter ')' : tokenize xs
tokenize (' ':xs) = Splitter ' ' : tokenize xs
tokenize (',':xs) = Splitter ',' : tokenize xs
tokenize (x:xs)   = Number [x]   : tokenize xs

reduceTokens :: [Token] -> [Token]
reduceTokens []                                   = []
reduceTokens (Number digit1 : Number digit2 : xs) = reduceTokens (Number (digit1 ++ digit2) : xs)
reduceTokens (v:xs)                               = v : reduceTokens xs

parsing :: [Token] -> ParsingPixel
parsing [Splitter '(', Number x, Splitter ',', Number y, Splitter ')', Splitter ' ', Splitter '(', Number r, Splitter ',', Number g, Splitter ',', Number b, Splitter ')']
           = ParsingPixel (readMaybe x, readMaybe y) (readMaybe r, readMaybe g, readMaybe b)
parsing a  = throw FileParse --  $ InputError $ "There was an Error: " ++ show a

finalPixelCheck :: ParsingPixel -> Pixel
finalPixelCheck (ParsingPixel (Nothing, _      ) (_      , _      , _      )) = throw FileParse
finalPixelCheck (ParsingPixel (_      , Nothing) (_      , _      , _      )) = throw FileParse
finalPixelCheck (ParsingPixel (_      , _      ) (Nothing, _      , _      )) = throw FileParse
finalPixelCheck (ParsingPixel (_      , _      ) (_      , Nothing, _      )) = throw FileParse
finalPixelCheck (ParsingPixel (_      , _      ) (_      , _      , Nothing)) = throw FileParse
finalPixelCheck (ParsingPixel (Just x, Just y) (Just r, Just g, Just b)) | r < 0 || r > 255 = throw FileParseColorError
                                                                         | g < 0 || g > 255 = throw FileParseColorError
                                                                         | b < 0 || b > 255 = throw FileParseColorError
                                                                         | otherwise = Pixel (x, y) (r, g, b)