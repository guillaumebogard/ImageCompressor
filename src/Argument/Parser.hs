module           Argument.Parser                  ( CompressorArgs(..)
                                                  , NbColors(..)
                                                  , Limit(..)
                                                  , Filepath
                                                  , parse
                                                  ) where

import           Text.Read                        ( readMaybe )
import           Control.Exception                ( throw )

import qualified Argument.Lexer            as AL  ( Token(..)
                                                  , tokenize )
import qualified Argument.Parser.Exception as APE ( ArgumentParserException(..) )


newtype NbColors         = NbColors Int
    deriving Read

newtype Limit            = Limit    Float
    deriving Read

type Filepath            =          String

data    CompressorArgs   = CompressorArgs          NbColors         Limit         Filepath

data    CompressorArgsIn = CompressorArgsIn (Maybe NbColors) (Maybe Limit) (Maybe Filepath)

parse :: [String] -> CompressorArgs
parse = parseTokenized . AL.tokenize

parseTokenized :: [AL.Token] -> CompressorArgs
parseTokenized = parseTokenized' (CompressorArgsIn Nothing Nothing Nothing) . filter helpHandler . filter unknownOptionHandler

helpHandler :: AL.Token -> Bool
helpHandler AL.HelpOption = throw APE.ArgumentParserHelpException
helpHandler _             = True

unknownOptionHandler :: AL.Token -> Bool
unknownOptionHandler (AL.UnknownOption opt) = throw $ APE.ArgumentParserException $ "Unknown option: '" ++ opt ++ "'"
unknownOptionHandler _                      = True

parseTokenized' :: CompressorArgsIn -> [AL.Token] -> CompressorArgs
parseTokenized' argsIn []                                      = getCompressorArgsFromIn argsIn
parseTokenized' argsIn (AL.NbColorsOption :AL.Argument arg:xs) = parseTokenized' (setNbColorsIn argsIn arg) xs
parseTokenized' argsIn (AL.LimitOption    :AL.Argument arg:xs) = parseTokenized' (setLimitIn    argsIn arg) xs
parseTokenized' argsIn (AL.FilepathOption :AL.Argument arg:xs) = parseTokenized' (setFilepathIn argsIn arg) xs
parseTokenized' _      _                                       = throw $ APE.ArgumentParserException "Invalid arguments, retry with -h"

setNbColorsIn :: CompressorArgsIn -> String -> CompressorArgsIn
setNbColorsIn (CompressorArgsIn _ limit filepath) arg = CompressorArgsIn (readMaybe arg) limit filepath

setLimitIn :: CompressorArgsIn -> String -> CompressorArgsIn
setLimitIn (CompressorArgsIn nbColors _ filepath) arg = CompressorArgsIn nbColors (readMaybe arg) filepath

setFilepathIn :: CompressorArgsIn -> String -> CompressorArgsIn
setFilepathIn (CompressorArgsIn nbColors limit _) arg = CompressorArgsIn nbColors limit (Just arg)

getCompressorArgsFromIn :: CompressorArgsIn -> CompressorArgs
getCompressorArgsFromIn (CompressorArgsIn Nothing         _            _              ) = throw $ APE.ArgumentParserException "Invalid arguments, missing -n option"
getCompressorArgsFromIn (CompressorArgsIn _               Nothing      _              ) = throw $ APE.ArgumentParserException "Invalid arguments, missing -l option"
getCompressorArgsFromIn (CompressorArgsIn _               _            Nothing        ) = throw $ APE.ArgumentParserException "Invalid arguments, missing -f option"
getCompressorArgsFromIn (CompressorArgsIn (Just nbColors) (Just limit) (Just filepath)) = CompressorArgs nbColors limit filepath
