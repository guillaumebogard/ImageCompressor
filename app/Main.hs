--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- Main
--

module Main ( main ) where

import Control.Exception    ( handle )
import System.Exit          ( ExitCode(ExitFailure)
                            , exitWith
                            )
import System.Environment   ( getArgs )
import Errors               ( MyError(..) )
import Usage                ( printUsage )
import CompressorConf       ( getCompressorConf )
import Parsing              ( Conf(..)
                            , parseArgs
                            , getFilePath
                            )

main :: IO ()
main = handle
        handleErrors
        (getArgs >>= launchApp)

launchApp :: [String] -> IO ()
launchApp []         = printUsage
launchApp ["-h"]     = printUsage
launchApp ["-help"]  = printUsage
launchApp ["--help"] = printUsage
launchApp args       = let conf = parseArgs args in readLines (getFilePath conf) >>= print . getCompressorConf conf

readLines :: String -> IO [String]
readLines = fmap lines . readFile 

handleErrors :: MyError -> IO ()
handleErrors (ArgumentError e)      = putStrLn e >> exitWith (ExitFailure 84)
handleErrors FileParse           = putStrLn "Error while parsing the file." >> exitWith (ExitFailure 84)
handleErrors FileParseColorError = putStrLn "Error while parsing the file: a color must be between 0 and 255." >> exitWith (ExitFailure 84)
