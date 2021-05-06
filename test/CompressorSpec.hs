--
-- EPITECH PROJECT, 2021
-- B-FUN-400-BDX-4-1-compressor-guillaume.bogard-coquard
-- File description:
-- CompressorSpec
--

module CompressorSpec ( spec ) where

import Test.Hspec   ( Spec
                    , it
                    , shouldBe
                    , shouldThrow
                    , anyException)

import Control.Exception (evaluate)
import System.Random

import Compressor
import CompressorConf
import FileParsing.CreatePixel
import FileParsing.Pixel
import Cluster.Cluster
import Vector.Vector
import RandomManager

spec :: Spec
spec = do
    let pixels = [   Pixel (Vector2 (0, 0)) (Vector3 (33, 18, 109)) -- 0 A
                   , Pixel (Vector2 (0, 1)) (Vector3 (33, 18, 109)) -- 1 B
                   , Pixel (Vector2 (0, 2)) (Vector3 (33, 21, 109)) -- 2 C
                   , Pixel (Vector2 (0, 3)) (Vector3 (33, 21, 112)) -- 3 D
                   , Pixel (Vector2 (0, 4)) (Vector3 (33, 25, 112)) -- 4 E
                   , Pixel (Vector2 (0, 5)) (Vector3 (33, 32, 112)) -- 5 F
                   , Pixel (Vector2 (1, 0)) (Vector3 (33, 18, 109)) -- 6 G
                   , Pixel (Vector2 (1, 1)) (Vector3 (35, 18, 109)) -- 7 H
                   , Pixel (Vector2 (1, 2)) (Vector3 (35, 21, 109)) -- 8 I
                   , Pixel (Vector2 (1, 3)) (Vector3 (38, 21, 112)) -- 9 J
                 ]
    let seed = mkStdGen 230844504580

    it "makeNbRandomsUnique" $ makeNbRandomsUnique seed 0 9   `shouldBe` []
    it "makeNbRandomsUnique" $ makeNbRandomsUnique seed 2 9   `shouldBe` [8, 7]
    it "makeNbRandomsUnique" $ makeNbRandomsUnique seed 4 100 `shouldBe` [52, 6, 90, 68]
    it "makeNbRandomsUnique" $ makeNbRandomsUnique seed 2 2   `shouldBe` [0, 2]
    it "makeNbRandomsUnique" $ makeNbRandomsUnique seed 7 7   `shouldBe` [2, 1, 6, 7, 0, 5, 4]

    it "extractFirstClustersPos" $ extractFirstClustersPos []     []        `shouldBe` []
    it "extractFirstClustersPos" $ extractFirstClustersPos []     [1]       `shouldBe` []
    it "extractFirstClustersPos" $ extractFirstClustersPos pixels [1, 7]    `shouldBe` [Vector3 (33, 18, 109), Vector3 (35, 18, 109)]
    it "extractFirstClustersPos" $ extractFirstClustersPos pixels [3, 0, 9] `shouldBe` [Vector3 (33,18,109),   Vector3 (33,21,112), Vector3 (38,21,112)]

    it "insertPixel" $ insertPixel []
                                (Pixel (Vector2 (0, 0)) (Vector3 (33, 21, 109))) 0
                                `shouldBe` []
    it "insertPixel" $ insertPixel [((Vector3 (33.0,18.0,109.0), Vector3 (0, 0, 0)), []), ((Vector3 (34.166668,23.5,111.0), Vector3 (0, 0, 0)), [])]
                                (Pixel (Vector2 (0, 0)) (Vector3 (33, 21, 109))) 0
                                `shouldBe` [((Vector3 (33.0,18.0,109.0), Vector3 (0, 0, 0)), [Pixel (Vector2 (0, 0)) (Vector3 (33, 21, 109))]), ((Vector3 (34.166668,23.5,111.0), Vector3 (0, 0, 0)), [])]
    it "insertPixel" $ insertPixel [((Vector3 (33.0,18.0,109.0), Vector3 (0, 0, 0)), []), ((Vector3 (34.166668,23.5,111.0), Vector3 (0, 0, 0)), [])]
                                (Pixel (Vector2 (0, 0)) (Vector3 (33, 21, 109))) 1
                                `shouldBe` [((Vector3 (33.0,18.0,109.0), Vector3 (0, 0, 0)), []), ((Vector3 (34.166668,23.5,111.0), Vector3 (0, 0, 0)), [Pixel (Vector2 (0, 0)) (Vector3 (33, 21, 109))])]

    it "linkPixelsToClusters" $ linkPixelsToClusters pixels ([Vector3 (34.125, 19.5, 109.75), Vector3 (33, 28.5, 112)], [Vector3 (33, 21, 112), Vector3 (33, 25, 112)]) `shouldBe` [
            Cluster (Vector3 (34.125, 19.5, 109.75))
            [
                Pixel (Vector2 (0, 0)) (Vector3 (33, 18, 109)),
                Pixel (Vector2 (0, 1)) (Vector3 (33, 18, 109)),
                Pixel (Vector2 (0, 2)) (Vector3 (33, 21, 109)),
                Pixel (Vector2 (0, 3)) (Vector3 (33, 21, 112)),
                Pixel (Vector2 (1, 0)) (Vector3 (33, 18, 109)),
                Pixel (Vector2 (1, 1)) (Vector3 (35, 18, 109)),
                Pixel (Vector2 (1, 2)) (Vector3 (35, 21, 109)),
                Pixel (Vector2 (1, 3)) (Vector3 (38, 21, 112))
            ],
            Cluster (Vector3 (33, 28.5, 112))
            [
                Pixel (Vector2 (0, 4)) (Vector3 (33, 25, 112)),
                Pixel (Vector2 (0, 5)) (Vector3 (33, 32, 112))
            ]
        ]
    it "linkPixelsToClusters" $ linkPixelsToClusters pixels ([Vector3 (33, 32, 112), Vector3 (34, 20.11111, 110)], [Vector3 (33, 32, 112), Vector3 (35, 21, 109)]) `shouldBe` [
            Cluster (Vector3 (33.0,32.0,112.0))
            [
                Pixel (Vector2 (0, 5)) (Vector3 (33, 32, 112))
            ],
            Cluster (Vector3 (34.0,20.11111,110.0))
            [
                Pixel (Vector2 (0, 0)) (Vector3 (33, 18, 109)),
                Pixel (Vector2 (0, 1)) (Vector3 (33, 18, 109)),
                Pixel (Vector2 (0, 2)) (Vector3 (33, 21, 109)),
                Pixel (Vector2 (0, 3)) (Vector3 (33, 21, 112)),
                Pixel (Vector2 (0, 4)) (Vector3 (33, 25, 112)),
                Pixel (Vector2 (1, 0)) (Vector3 (33, 18, 109)),
                Pixel (Vector2 (1, 1)) (Vector3 (35, 18, 109)),
                Pixel (Vector2 (1, 2)) (Vector3 (35, 21, 109)),
                Pixel (Vector2 (1, 3)) (Vector3 (38, 21, 112))
            ]
        ]

    it "safeDivToFloat" $ safeDivToFloat 2 0 `shouldBe` 0

    it "generateMoves" $ generateMoves [] [] `shouldBe` []
    it "generateMoves" $ generateMoves [Vector3 (0, 0, 0)] [] `shouldBe` []
    it "generateMoves" $ generateMoves [Vector3 (33.0,21.11111,113.5), Vector3 (33.0,32.0,112.0)] [(9, Vector3 (306, 181, 990)), (1, Vector3 (33, 32, 112))] `shouldBe` [Vector3 (1, -1, -3.5), Vector3 (0, 0, 0)]

    it "filterMoves" $ filterMoves 0.8 [] `shouldBe` []
    it "filterMoves" $ filterMoves 0.8 [Vector3 (1, -1, -3.5),   Vector3 (0, 0, 0)]   `shouldBe` [Vector3 (1, -1, -3.5), Vector3 (0, 0, 0)]
    it "filterMoves" $ filterMoves 1.9 [Vector3 (0, 2.1, 0),     Vector3 (0, 2, 0)]   `shouldBe` [Vector3 (0, 2.1, 0),   Vector3 (0, 2, 0)]
    it "filterMoves" $ filterMoves 2   [Vector3 (0, 2.1, 0),     Vector3 (0, 2, 0)]   `shouldBe` [Vector3 (0, 2.1, 0),   Vector3 (0, 2, 0)]
    it "filterMoves" $ filterMoves 2.1 [Vector3 (0, 2.1, 0),     Vector3 (0, 2, 0)]   `shouldBe` []
    it "filterMoves" $ filterMoves 0.8 [Vector3 (0, -0.5, -0.1), Vector3 (0, 0.6, 0)] `shouldBe` []

    it "findClosestCluster" $ findClosestCluster [] (Vector3 (33, 18, 109)) `shouldBe` -1
    it "findClosestCluster" $ findClosestCluster [Vector3 (34.0, 20.11111, 110.0), Vector3 (33.0, 32.0, 112.0)] (Vector3 (33, 18, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (34.0, 20.11111, 110.0), Vector3 (33.0, 32.0, 112.0)] (Vector3 (33, 18, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (34.0, 20.11111, 110.0), Vector3 (33.0, 32.0, 112.0)] (Vector3 (33, 21, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (34.0, 20.11111, 110.0), Vector3 (33.0, 32.0, 112.0)] (Vector3 (33, 21, 112)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (34.0, 20.11111, 110.0), Vector3 (33.0, 32.0, 112.0)] (Vector3 (33, 25, 112)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (34.0, 20.11111, 110.0), Vector3 (33.0, 32.0, 112.0)] (Vector3 (33, 18, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (34.0, 20.11111, 110.0), Vector3 (33.0, 32.0, 112.0)] (Vector3 (35, 18, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (34.0, 20.11111, 110.0), Vector3 (33.0, 32.0, 112.0)] (Vector3 (35, 21, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (34.0, 20.11111, 110.0), Vector3 (33.0, 32.0, 112.0)] (Vector3 (38, 21, 112)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (34.0, 20.11111, 110.0), Vector3 (33.0, 32.0, 112.0)] (Vector3 (33, 32, 112)) `shouldBe` 1

    it "findClosestCluster" $ findClosestCluster [Vector3 (34.125, 19.5, 109.75), Vector3 (33, 28.5, 112)] (Vector3 (33, 18, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (34.125, 19.5, 109.75), Vector3 (33, 28.5, 112)] (Vector3 (33, 18, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (34.125, 19.5, 109.75), Vector3 (33, 28.5, 112)] (Vector3 (33, 21, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (34.125, 19.5, 109.75), Vector3 (33, 28.5, 112)] (Vector3 (33, 21, 112)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (34.125, 19.5, 109.75), Vector3 (33, 28.5, 112)] (Vector3 (33, 18, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (34.125, 19.5, 109.75), Vector3 (33, 28.5, 112)] (Vector3 (35, 18, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (34.125, 19.5, 109.75), Vector3 (33, 28.5, 112)] (Vector3 (35, 21, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (34.125, 19.5, 109.75), Vector3 (33, 28.5, 112)] (Vector3 (38, 21, 112)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (34.125, 19.5, 109.75), Vector3 (33, 28.5, 112)] (Vector3 (33, 25, 112)) `shouldBe` 1
    it "findClosestCluster" $ findClosestCluster [Vector3 (34.125, 19.5, 109.75), Vector3 (33, 28.5, 112)] (Vector3 (33, 32, 112)) `shouldBe` 1

    it "findClosestCluster" $ findClosestCluster [Vector3 (33.0,18.0,109.0), Vector3 (34.166668,23.5,111.0)] (Vector3 (33, 18, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (33.0,18.0,109.0), Vector3 (34.166668,23.5,111.0)] (Vector3 (33, 18, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (33.0,18.0,109.0), Vector3 (34.166668,23.5,111.0)] (Vector3 (33, 18, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (33.0,18.0,109.0), Vector3 (34.166668,23.5,111.0)] (Vector3 (35, 18, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (33.0,18.0,109.0), Vector3 (34.166668,23.5,111.0)] (Vector3 (33, 21, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (33.0,18.0,109.0), Vector3 (34.166668,23.5,111.0)] (Vector3 (33, 21, 112)) `shouldBe` 1
    it "findClosestCluster" $ findClosestCluster [Vector3 (33.0,18.0,109.0), Vector3 (34.166668,23.5,111.0)] (Vector3 (33, 25, 112)) `shouldBe` 1
    it "findClosestCluster" $ findClosestCluster [Vector3 (33.0,18.0,109.0), Vector3 (34.166668,23.5,111.0)] (Vector3 (33, 32, 112)) `shouldBe` 1
    it "findClosestCluster" $ findClosestCluster [Vector3 (33.0,18.0,109.0), Vector3 (34.166668,23.5,111.0)] (Vector3 (35, 21, 109)) `shouldBe` 1
    it "findClosestCluster" $ findClosestCluster [Vector3 (33.0,18.0,109.0), Vector3 (34.166668,23.5,111.0)] (Vector3 (38, 21, 112)) `shouldBe` 1

    it "findClosestCluster" $ findClosestCluster [Vector3 (33.57143,19.285715,109.42857), Vector3 (33.0,28.5,112.0)] (Vector3 (33, 18, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (33.57143,19.285715,109.42857), Vector3 (33.0,28.5,112.0)] (Vector3 (33, 18, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (33.57143,19.285715,109.42857), Vector3 (33.0,28.5,112.0)] (Vector3 (33, 21, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (33.57143,19.285715,109.42857), Vector3 (33.0,28.5,112.0)] (Vector3 (33, 21, 112)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (33.57143,19.285715,109.42857), Vector3 (33.0,28.5,112.0)] (Vector3 (33, 18, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (33.57143,19.285715,109.42857), Vector3 (33.0,28.5,112.0)] (Vector3 (35, 18, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (33.57143,19.285715,109.42857), Vector3 (33.0,28.5,112.0)] (Vector3 (35, 21, 109)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (33.57143,19.285715,109.42857), Vector3 (33.0,28.5,112.0)] (Vector3 (38, 21, 112)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (33.57143,19.285715,109.42857), Vector3 (33.0,28.5,112.0)] (Vector3 (33, 25, 112)) `shouldBe` 1
    it "findClosestCluster" $ findClosestCluster [Vector3 (33.57143,19.285715,109.42857), Vector3 (33.0,28.5,112.0)] (Vector3 (33, 32, 112)) `shouldBe` 1

    it "findClosestCluster" $ findClosestCluster [Vector3 (4, 4, 0), Vector3 (4, 4, 8), Vector3 (4, 4, 4), Vector3 (4, 4, 12)] (Vector3 (8, 4, -2)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (4, 4, 0), Vector3 (4, 4, 8), Vector3 (4, 4, 4), Vector3 (4, 4, 12)] (Vector3 (8, 4, 2) ) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 (4, 4, 0), Vector3 (4, 4, 8), Vector3 (4, 4, 4), Vector3 (4, 4, 12)] (Vector3 (8, 4, 6) ) `shouldBe` 1
    it "findClosestCluster" $ findClosestCluster [Vector3 (4, 4, 0), Vector3 (4, 4, 8), Vector3 (4, 4, 4), Vector3 (4, 4, 12)] (Vector3 (8, 4, 7) ) `shouldBe` 1
    it "findClosestCluster" $ findClosestCluster [Vector3 (4, 4, 0), Vector3 (4, 4, 8), Vector3 (4, 4, 4), Vector3 (4, 4, 12)] (Vector3 (8, 4, 10)) `shouldBe` 1
    it "findClosestCluster" $ findClosestCluster [Vector3 (4, 4, 0), Vector3 (4, 4, 8), Vector3 (4, 4, 4), Vector3 (4, 4, 12)] (Vector3 (8, 4, 3) ) `shouldBe` 2
    it "findClosestCluster" $ findClosestCluster [Vector3 (4, 4, 0), Vector3 (4, 4, 8), Vector3 (4, 4, 4), Vector3 (4, 4, 12)] (Vector3 (8, 4, 11)) `shouldBe` 3
    it "findClosestCluster" $ findClosestCluster [Vector3 (4, 4, 0), Vector3 (4, 4, 8), Vector3 (4, 4, 4), Vector3 (4, 4, 12)] (Vector3 (8, 4, 17)) `shouldBe` 3

    it "insertColor" $ insertColor [] (Vector3 (0, 0, 0)) 0 `shouldBe` []
    it "insertColor" $ insertColor [(9, Vector3 (306, 181, 990)), (1, Vector3 (33, 32, 112))] (Vector3 (0,  0,   0))   0 `shouldBe` [(10, Vector3 (306, 181, 990)), (1, Vector3 (33, 32, 112))]
    it "insertColor" $ insertColor [(9, Vector3 (306, 181, 990)), (1, Vector3 (33, 32, 112))] (Vector3 (33, 244, 109)) 0 `shouldBe` [(10, Vector3 (306+33, 181+244, 990+109)), (1, Vector3 (33, 32, 112))]
    it "insertColor" $ insertColor [(9, Vector3 (306, 181, 990)), (1, Vector3 (33, 32, 112))] (Vector3 (33, 244, 109)) 1 `shouldBe` [(9,  Vector3 (306, 181, 990)), (2, Vector3 (33+33, 32+244, 112+109))]

    it "generateEmptyTotals" $ generateEmptyTotals 0 `shouldBe` []
    it "generateEmptyTotals" $ generateEmptyTotals 1 `shouldBe` [(0, Vector3 (0, 0, 0))]
    it "generateEmptyTotals" $ generateEmptyTotals 2 `shouldBe` [(0, Vector3 (0, 0, 0)), (0, Vector3 (0, 0, 0))]
    it "generateEmptyTotals" $ generateEmptyTotals 4 `shouldBe` [(0, Vector3 (0, 0, 0)), (0, Vector3 (0, 0, 0)), (0, Vector3 (0, 0, 0)), (0, Vector3 (0, 0, 0))]

    it "applyMove" $ applyMove  [Vector3 (33, 18, 109), Vector3 (34.166668, 23.5, 111)]
                                []
                                `shouldBe`
                                [Vector3 (33, 18, 109), Vector3 (34.166668, 23.5, 111)]
    it "applyMove" $ applyMove  [Vector3 (33, 18, 109), Vector3 (34.166668, 23.5, 111)]
                                [Vector3 (0,  0,  0),   Vector3 (1, -1, 4)]
                                `shouldBe`
                                [Vector3 (33,  18, 109),   Vector3 (34.166668+1, 23.5-1, 111+4)]
    it "applyMove" $ applyMove  [Vector3 (33,  18, 109),   Vector3 (34.166668,   23.5,   111)]
                                [Vector3 (1.5, -10, 4.12), Vector3 (1, -1, 4)]
                                `shouldBe`
                                [Vector3 (33+1.5, 18-10, 109+4.12), Vector3 (34.166668+1, 23.5-1, 111+4)]
    it "applyMove" $ applyMove  [Vector3 (33,     18,    109),      Vector3 (1, 2, 3), Vector3 (34.166668, 23.5, 111)]
                                [Vector3 (1.5,   -10,    4.12),     Vector3 (0, 0, 0), Vector3 (1, -1, 4)]
                                `shouldBe`
                                [Vector3 (33+1.5, 18-10, 109+4.12), Vector3 (1, 2, 3), Vector3 (34.166668+1, 23.5-1, 111+4)]

    it "adjustClusters" $ adjustClusters 0.8 pixels  [Vector3 (33, 18,   109), Vector3 (35, 18, 109)]
                                        `shouldBe`  ([Vector3 (33, 28.5, 112), Vector3 (34.125, 19.5, 109.75)], [Vector3 (33, 24.75, 111.25), Vector3 (34.5, 19, 109.5)])
    it "adjustClusters" $ adjustClusters 0.8 pixels  [Vector3 (33, 32,   112), Vector3 (35, 21, 109)]
                                        `shouldBe`  ([Vector3 (33, 32,   112), Vector3 (34, 20.11111, 110)],    [Vector3 (33, 32, 112),       Vector3 (35, 21, 109)])
    it "adjustClusters" $ adjustClusters 0.8 pixels  [Vector3 (33, 32,   112), Vector3 (34, 20.11111, 110)]
                                        `shouldBe`  ([Vector3 (33, 32,   112), Vector3 (34, 20.11111, 110)],    [Vector3 (33, 32, 112),       Vector3 (34, 20.11111, 110)])
    it "adjustClusters" $ adjustClusters 0.8 pixels  [Vector3 (33, 18,   109), Vector3 (33, 18, 109)]
                                        `shouldBe`  ([Vector3 (34.166668, 23.5, 111) , Vector3 (33.5,18, 109)], [Vector3 (33.9, 21.3, 110.2), Vector3 (33, 18, 109)])

    it "compress" $ compress seed (CompressorConf 0 0.8 pixels) `shouldBe` []
    it "compress" $ compress seed (CompressorConf 2 0.8 []    ) `shouldBe` []
    it "compress" $ compress seed (CompressorConf 2 0.8 pixels) `shouldBe` [
            Cluster (Vector3 (33.5, 18, 109))
            [
                Pixel (Vector2 (0, 0)) (Vector3 (33, 18, 109)),
                Pixel (Vector2 (0, 1)) (Vector3 (33, 18, 109)),
                Pixel (Vector2 (1, 0)) (Vector3 (33, 18, 109)),
                Pixel (Vector2 (1, 1)) (Vector3 (35, 18, 109))
            ],
            Cluster (Vector3 (34.166668, 23.5, 111))
            [
                Pixel (Vector2 (0, 2)) (Vector3 (33, 21, 109)),
                Pixel (Vector2 (0, 3)) (Vector3 (33, 21, 112)),
                Pixel (Vector2 (0, 4)) (Vector3 (33, 25, 112)),
                Pixel (Vector2 (0, 5)) (Vector3 (33, 32, 112)),
                Pixel (Vector2 (1, 2)) (Vector3 (35, 21, 109)),
                Pixel (Vector2 (1, 3)) (Vector3 (38, 21, 112))
            ]
        ]
