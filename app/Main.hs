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
import System.Random        ( newStdGen, mkStdGen )
import Errors               ( MyError(..) )
import Usage                ( printUsage )
import CompressorConf       ( getCompressorConf )
import Parsing              ( Conf(..)
                            , parseArgs
                            , getFilePath
                            )
import Compressor           ( compress, Cluster )

main :: IO ()
main = handle
        handleErrors
        (getArgs >>= launchApp)

launchApp :: [String] -> IO ()
launchApp []         = printUsage
launchApp ["-h"]     = printUsage
launchApp ["-help"]  = printUsage
launchApp ["--help"] = printUsage
launchApp args       = let conf@(Conf _ _ filePath) = parseArgs args in readLines filePath >>= (\fileContent -> newStdGen >>= (\seed -> printClusters $ compress seed $ getCompressorConf conf fileContent))
-- launchApp args       = let conf@(Conf _ _ filePath) = parseArgs args in readLines filePath >>= print . compress (mkStdGen 230844504580) . getCompressorConf conf
-- launchApp args       = let conf@(Conf _ _ filePath) = parseArgs args in readLines filePath >>= print . getCompressorConf conf

printClusters :: [Cluster] -> IO ()
printClusters = mapM_ (\((pos, prev), ps) -> putStrLn "--" >> print pos >> putStrLn "-" >> mapM_ print ps)

readLines :: String -> IO [String]
readLines = fmap lines . readFile 

handleErrors :: MyError -> IO ()
handleErrors (ArgumentError e)      = putStrLn e >> exitWith (ExitFailure 84)
handleErrors FileParse           = putStrLn "Error while parsing the file." >> exitWith (ExitFailure 84)
handleErrors FileParseColorError = putStrLn "Error while parsing the file: a color must be between 0 and 255." >> exitWith (ExitFailure 84)
