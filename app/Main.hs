module           Main                             ( main ) where

import           System.Environment               ( getArgs )
import           System.Exit                      ( ExitCode(ExitFailure)
                                                  , exitWith
                                                  , exitSuccess )
import           Control.Exception                ( Handler(..)
                                                  , catches )

import qualified Argument.Parser.Exception as APE


main :: IO ()
main = (getArgs >>= launchImageCompressor) `catches` [Handler exceptionHandlerAPE]

launchImageCompressor :: [String] -> IO ()
launchImageCompressor _ = print "launchImageCompressor"

exceptionHandlerAPE :: APE.ArgumentParserException -> IO ()
exceptionHandlerAPE APE.ArgumentParserHelpException = print APE.ArgumentParserHelpException >> exitSuccess
exceptionHandlerAPE exception                       = print exception                       >> exitWith (ExitFailure 84)
