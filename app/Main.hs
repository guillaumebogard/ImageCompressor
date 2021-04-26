--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- Main
--

module Main ( main ) where

import Control.Exception ( handle )
import System.Exit ( ExitCode(ExitFailure)
                   , exitWith
                   )
import System.Environment   ( getArgs )
import Errors ( MyError(..) )
import Usage ( printUsage )
import Parsing ( parseArgs, Conf(..) )
import CompressorConf ( getCompressorConf )

handleErrors :: MyError -> IO ()
handleErrors (InputError e) = putStrLn e >> exitWith (ExitFailure 84)
handleErrors (OtherError e) = putStrLn e >> exitWith (ExitFailure 84)

getFilePath :: Conf -> String
getFilePath (Conf _ _ f) = f

readLines :: String -> IO [String]
readLines = fmap lines . readFile 

main :: IO ()
main = handle
        handleErrors
        (getArgs >>= (\args -> let conf = parseArgs args in readLines (getFilePath conf) >>= print . getCompressorConf conf))
