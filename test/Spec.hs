--
-- EPITECH PROJECT, 2021
-- B-FUN-400-BDX-4-1-compressor-guillaume.bogard-coquard
-- File description:
-- Spec
--

module Main                 ( main ) where

import Test.Hspec           ( hspec
                            , Spec
                            , describe )

import ArgumentParsingSpec  ( spec )
import ClusterSpec          ( spec )
import CompressorSpec       ( spec )
import FileParsingSpec      ( spec )
import RandomManagerSpec    ( spec )
import VectorSpec           ( spec )

spec :: Spec
spec = do
    describe "ArgumentParsing"  ArgumentParsingSpec.spec
    describe "Cluster"          ClusterSpec.spec
    describe "Compressor"       CompressorSpec.spec
    describe "FileParsing"      FileParsingSpec.spec
    describe "RandomManager"    RandomManagerSpec.spec
    describe "Vector"           VectorSpec.spec

main :: IO ()
main = hspec Main.spec
