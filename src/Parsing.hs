--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- Parsing
--

module Parsing ( parseArgs ) where

import Control.Exception    ( throw )
import Text.Read            ( readMaybe )
import Errors               ( MyError(..) )
import Lexing               ( TOKEN(..)
                            , tokenize
                            )

type NumberColors   = Int
type NumberLimit    = Int
type StringFilepath = String

data ParsingConf = ParsingConf (Maybe NumberColors) (Maybe NumberLimit) (Maybe StringFilepath)

data Conf        = Conf NumberColors NumberLimit StringFilepath
instance Show Conf where
    show (Conf n l f) = "Nb Colors: " ++ show n ++ ", Limit: " ++ show l ++ ", File Path: " ++ show f

parseArgs :: [String] -> Conf
parseArgs args = finalConfCheck $ parsing (ParsingConf Nothing Nothing Nothing) $ tokenize args

finalConfCheck :: ParsingConf -> Conf
finalConfCheck (ParsingConf Nothing         _              _                    ) = throw $ InputError "The number of colors must be set."
finalConfCheck (ParsingConf _               Nothing        _                    ) = throw $ InputError "The convergence limit must be set."
finalConfCheck (ParsingConf _               _              Nothing              ) = throw $ InputError "A file path must be set."
finalConfCheck (ParsingConf (Just nbColors) (Just nbLimit) (Just stringFilepath)) = Conf nbColors nbLimit stringFilepath

parsing :: ParsingConf -> [TOKEN] -> ParsingConf
parsing parsingConf []                            = parsingConf
parsing parsingConf (COLORS   : (Value val) : xs) = parsing (setColors   parsingConf val) xs
parsing parsingConf (LIMIT    : (Value val) : xs) = parsing (setLimit    parsingConf val) xs
parsing parsingConf (FILEPATH : (Value val) : xs) = parsing (setFilepath parsingConf val) xs
parsing _           _                             = throw $ InputError "Invalid argument(s) given."

setColors :: ParsingConf -> String -> ParsingConf
setColors (ParsingConf nbColors nbLimit stringFilepath)   val = ParsingConf (readMaybe val) nbLimit stringFilepath

setLimit :: ParsingConf -> String -> ParsingConf
setLimit (ParsingConf nbColors nbLimit stringFilepath)    val = ParsingConf nbColors (readMaybe val) stringFilepath

setFilepath :: ParsingConf -> String -> ParsingConf
setFilepath (ParsingConf nbColors nbLimit stringFilepath) val = ParsingConf nbColors nbLimit (Just val)
