--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- Lexing
--

module ArgumentParsing.Lexing ( TOKEN (..)
                              , tokenize
                              ) where

data TOKEN = HELP | COLORS | LIMIT | FILEPATH | Value String

tokenize :: [String] -> [TOKEN]
tokenize []            = []
tokenize ("-h":xs)     = HELP : tokenize xs
tokenize ("-help":xs)  = HELP : tokenize xs
tokenize ("--help":xs) = HELP : tokenize xs
tokenize ("-n":xs)     = COLORS : tokenize xs
tokenize ("-l":xs)     = LIMIT : tokenize xs
tokenize ("-f":xs)     = FILEPATH : tokenize xs
tokenize (x:xs)        = Value x : tokenize xs
