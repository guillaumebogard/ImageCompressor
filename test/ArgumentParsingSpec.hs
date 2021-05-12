--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- ArgumentParsingSpec
--

module ArgumentParsingSpec      ( spec ) where

import Test.Hspec               ( Spec
                                , it
                                , shouldBe
                                , shouldThrow
                                , anyException )

import Control.Exception        ( evaluate )

import ArgumentParsing.Parsing  ( parseArgs)
import Errors                   ( CompressorError(ArgumentError) )
import Usage                    ( usage )

spec :: Spec
spec = do
    it "parseArgs" $ evaluate (parseArgs ["-l", "0.8", "-f", "./example.txt"])               `shouldThrow` (== ArgumentError "Invalid number of colors.")
    it "parseArgs" $ evaluate (parseArgs ["-n", "-2",  "-l", "0.8", "-f", "./example.txt"])  `shouldThrow` (== ArgumentError "Invalid number of colors (should be 0 < n < +Inf)")
    it "parseArgs" $ evaluate (parseArgs ["-n", "2",   "-f", "./example.txt"])               `shouldThrow` (== ArgumentError "Invalid convergence limit.")
    it "parseArgs" $ evaluate (parseArgs ["-n", "2",   "-l", "A", "-f", "./example.txt"])    `shouldThrow` (== ArgumentError "Invalid convergence limit.")
    it "parseArgs" $ evaluate (parseArgs ["-n", "2",   "-l", "-f", "./example.txt"])         `shouldThrow` (== ArgumentError "Invalid given argument(s).")
    it "parseArgs" $ evaluate (parseArgs ["-n", "2",   "-l", "0.8"])                         `shouldThrow` (== ArgumentError "Invalid filepath.")
    it "parseArgs" $ evaluate (parseArgs ["-n", "2",   "-l", "0.8", "-f"])                   `shouldThrow` (== ArgumentError "Invalid given argument(s).")
