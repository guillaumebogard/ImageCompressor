--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- PixelLexing
--

module FileParsing.PixelLexing ( TOKEN (..)
                               , tokenize
                               ) where

data TOKEN = Splitter Char | Number String

tokenize :: String -> [TOKEN]
tokenize = foldr tokenize' []

tokenize' :: Char -> [TOKEN] -> [TOKEN]
tokenize' '(' t               = Splitter '('   : t
tokenize' ')' t               = Splitter ')'   : t
tokenize' ' ' t               = Splitter ' '   : t
tokenize' ',' t               = Splitter ','   : t
tokenize' x   (Number a : ts) = Number (x : a) : ts
tokenize' x   t               = Number [x]     : t
