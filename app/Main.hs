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
import System.Random        ( newStdGen )
import Errors               ( CompressorError(ArgumentError
                                             , FileParse
                                             , FileParseColorError
                                             )
                            )
import Usage
import CompressorConf       ( getCompressorConf )
import ArgumentParsing.Parsing
import Compressor
import Cluster.Cluster
import Vector.Vector

main :: IO ()
main = handle
        handleErrors
        (getArgs >>= launchApp . parseArgs)

launchApp :: Either String Conf -> IO ()
launchApp (Left str)                       = putStrLn str
launchApp (Right conf@(Conf _ _ filePath)) = readLines filePath >>= (\fileContent -> newStdGen >>= (\seed -> printClusters $ compress seed $ getCompressorConf conf fileContent))
-- launchApp args       = let conf@(Conf _ _ filePath) = parseArgs args in readLines filePath >>= print . compress (mkStdGen 230844504580) . getCompressorConf conf
-- launchApp args       = let conf@(Conf _ _ filePath) = parseArgs args in readLines filePath >>= print . getCompressorConf conf

printClusters :: [Cluster] -> IO ()
printClusters = mapM_ print

readLines :: String -> IO [String]
readLines = fmap lines . readFile 

handleErrors :: CompressorError -> IO ()
handleErrors (ArgumentError e)   = putStrLn e >> exitWith (ExitFailure 84)
handleErrors FileParse           = putStrLn "Error while parsing the file." >> exitWith (ExitFailure 84)
handleErrors FileParseColorError = putStrLn "Error while parsing the file: a color must be between 0 and 255." >> exitWith (ExitFailure 84)
