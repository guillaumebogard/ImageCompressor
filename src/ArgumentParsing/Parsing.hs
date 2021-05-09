--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- Parsing
--

module ArgumentParsing.Parsing ( Conf(..)
                               , NumberColors
                               , NumberLimit
                               , StringFilepath
                               , parseArgs
                               ) where

import Control.Exception      ( throw )
import Text.Read              ( readMaybe )

import Errors                 ( CompressorError(..) )
import ArgumentParsing.Lexing ( TOKEN(..)
                              , tokenize )
import Usage                  ( usage )

type NumberColors   = Int
type NumberLimit    = Float
type StringFilepath = String

data ParsingConf    = ParsingConf (Maybe NumberColors) (Maybe NumberLimit) (Maybe StringFilepath)
data Conf           = Conf         NumberColors         NumberLimit         StringFilepath

parseArgs :: [String] -> Either String Conf
parseArgs []   = Left usage
parseArgs args = finalConfCheck $ parsing (ParsingConf Nothing Nothing Nothing) $ tokenize args

parsing :: ParsingConf -> [TOKEN] -> Either String ParsingConf
parsing parsingConf []                            = Right   parsingConf
parsing _           (HELP     :               _ ) = Left    usage
parsing parsingConf (COLORS   : (Value val) : xs) = parsing (setColors   parsingConf val) xs
parsing parsingConf (LIMIT    : (Value val) : xs) = parsing (setLimit    parsingConf val) xs
parsing parsingConf (FILEPATH : (Value val) : xs) = parsing (setFilepath parsingConf val) xs
parsing _           _                             = throw $ ArgumentError "Invalid given argument(s)."

setColors :: ParsingConf -> String -> ParsingConf
setColors (ParsingConf _ nbLimit stringFilepath) val = ParsingConf (readMaybe val) nbLimit stringFilepath

setLimit :: ParsingConf -> String -> ParsingConf
setLimit (ParsingConf nbColors _ stringFilepath) val = ParsingConf nbColors (readMaybe val) stringFilepath

setFilepath :: ParsingConf -> String -> ParsingConf
setFilepath (ParsingConf nbColors nbLimit _)     val = ParsingConf nbColors nbLimit (Just val)

finalConfCheck :: Either String ParsingConf -> Either String Conf
finalConfCheck (Left  str                                                               ) = Left str
finalConfCheck (Right (ParsingConf Nothing         _              _                    )) = throw $ ArgumentError "Missing/Invalid number of colors."
finalConfCheck (Right (ParsingConf _               Nothing        _                    )) = throw $ ArgumentError "Missing/Invalid convergence limit."
finalConfCheck (Right (ParsingConf _               _              Nothing              )) = throw $ ArgumentError "Missing/Invalid file path."
finalConfCheck (Right (ParsingConf (Just nbColors) (Just nbLimit) (Just stringFilepath))) = Right $ Conf nbColors nbLimit stringFilepath
