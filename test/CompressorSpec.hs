--
-- EPITECH PROJECT, 2021
-- B-FUN-400-BDX-4-1-compressor-guillaume.bogard-coquard
-- File description:
-- CompressorSpec
--

module CompressorSpec ( spec ) where

import Test.Hspec           ( Spec
                            , it
                            , shouldBe
                            , shouldThrow
                            , anyException )

import System.Random        ( mkStdGen )

import Cluster              ( Cluster(Cluster) )
import FileParsing.Pixel    ( Pixel(Pixel) )
import Vector               ( Vector3(Vector3), Vector2(Vector2) )
import Compressor           ( compress )
import CompressorConf       ( CompressorConf(CompressorConf) )

spec :: Spec
spec = do
    let pixels = [   Pixel (Vector2 0 0) (Vector3 33 18 109) -- 0 A
                   , Pixel (Vector2 0 1) (Vector3 33 18 109) -- 1 B
                   , Pixel (Vector2 0 2) (Vector3 33 21 109) -- 2 C
                   , Pixel (Vector2 0 3) (Vector3 33 21 112) -- 3 D
                   , Pixel (Vector2 0 4) (Vector3 33 25 112) -- 4 E
                   , Pixel (Vector2 0 5) (Vector3 33 32 112) -- 5 F
                   , Pixel (Vector2 1 0) (Vector3 33 18 109) -- 6 G
                   , Pixel (Vector2 1 1) (Vector3 35 18 109) -- 7 H
                   , Pixel (Vector2 1 2) (Vector3 35 21 109) -- 8 I
                   , Pixel (Vector2 1 3) (Vector3 38 21 112) -- 9 J
                 ]
    let seed = mkStdGen 230844504580

    it "compress" $ compress seed (CompressorConf 0 0.8 pixels) `shouldBe` []
    it "compress" $ compress seed (CompressorConf 2 0.8 []    ) `shouldBe` []
    it "compress" $ compress seed (CompressorConf 2 0.8 pixels) `shouldBe` [
            Cluster (Vector3 33.5 18 109)
            [
                Pixel (Vector2 0 0) (Vector3 33 18 109),
                Pixel (Vector2 0 1) (Vector3 33 18 109),
                Pixel (Vector2 1 0) (Vector3 33 18 109),
                Pixel (Vector2 1 1) (Vector3 35 18 109)
            ],
            Cluster (Vector3 34.166668 23.5 111)
            [
                Pixel (Vector2 0 2) (Vector3 33 21 109),
                Pixel (Vector2 0 3) (Vector3 33 21 112),
                Pixel (Vector2 0 4) (Vector3 33 25 112),
                Pixel (Vector2 0 5) (Vector3 33 32 112),
                Pixel (Vector2 1 2) (Vector3 35 21 109),
                Pixel (Vector2 1 3) (Vector3 38 21 112)
            ]
        ]
