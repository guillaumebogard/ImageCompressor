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
import Parsing ( parseArgs )

handleErrors :: MyError -> IO ()
handleErrors (InputError e) = putStrLn e >> exitWith (ExitFailure 84)
handleErrors (OtherError e) = putStrLn e >> exitWith (ExitFailure 84)

main :: IO ()
main = handle
        handleErrors
        (getArgs >>= print . parseArgs)
