--
-- EPITECH PROJECT, 2021
-- B-FUN-400-BDX-4-1-compressor-guillaume.bogard-coquard
-- File description:
-- Spec
--

module Main (main) where

import Test.Hspec   ( hspec
                    , Spec
                    , describe)

import CompressorSpec ( spec )

spec :: Spec
spec = do
    describe "Compressor" CompressorSpec.spec

main :: IO ()
main = hspec Main.spec
