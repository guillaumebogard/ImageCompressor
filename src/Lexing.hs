--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- Lexing
--

module Lexing (TOKEN(..)
             , tokenize)
             where


data TOKEN = COLORS | LIMIT | FILEPATH | Value String

tokenize :: [String] -> [TOKEN]
tokenize [] = []
tokenize ("-n":xs) = COLORS : tokenize xs
tokenize ("-l":xs) = LIMIT : tokenize xs
tokenize ("-f":xs) = FILEPATH : tokenize xs
tokenize (x:xs)    = Value x : tokenize xs
