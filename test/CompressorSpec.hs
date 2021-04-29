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
import CreatePixel

spec :: Spec
spec = do
    let pixels = [   Pixel (0, 0) (33, 18, 109) -- 0 A
                   , Pixel (0, 1) (33, 18, 109) -- 1 B
                   , Pixel (0, 2) (33, 21, 109) -- 2 C
                   , Pixel (0, 3) (33, 21, 112) -- 3 D
                   , Pixel (0, 4) (33, 25, 112) -- 4 E
                   , Pixel (0, 5) (33, 32, 112) -- 5 F
                   , Pixel (1, 0) (33, 18, 109) -- 6 G
                   , Pixel (1, 1) (35, 18, 109) -- 7 H
                   , Pixel (1, 2) (35, 21, 109) -- 8 I
                   , Pixel (1, 3) (38, 21, 112) -- 9 J
                 ]
    let seed = mkStdGen 230844504580

    it "makeNbRandoms" $ makeNbRandoms seed 0 9   `shouldBe` []
    it "makeNbRandoms" $ makeNbRandoms seed 2 9   `shouldBe` [7, 8]
    it "makeNbRandoms" $ makeNbRandoms seed 4 100 `shouldBe` [68,90,6,52]

    it "createFirstClustersPos" $ createFirstClustersPos [] []  `shouldBe` []
    it "createFirstClustersPos" $ createFirstClustersPos [] [1] `shouldBe` []
    it "createFirstClustersPos" $ createFirstClustersPos pixels [1, 7] `shouldBe` [(33, 18, 109), (35, 18, 109)]
    it "createFirstClustersPos" $ createFirstClustersPos pixels [3, 0, 9] `shouldBe` [(33,18,109), (33,21,112), (38,21,112)]

    it "insertPixel" $ insertPixel [] (Pixel (0, 0) (33, 21, 109)) 0 `shouldBe` []
    it "insertPixel" $ insertPixel [(((33.0,18.0,109.0), (0, 0, 0)), []), (((34.166668,23.5,111.0), (0, 0, 0)), [])] (Pixel (0, 0) (33, 21, 109)) 0 `shouldBe` [(((33.0,18.0,109.0), (0, 0, 0)), [Pixel (0, 0) (33, 21, 109)]), (((34.166668,23.5,111.0), (0, 0, 0)), [])]
    it "insertPixel" $ insertPixel [(((33.0,18.0,109.0), (0, 0, 0)), []), (((34.166668,23.5,111.0), (0, 0, 0)), [])] (Pixel (0, 0) (33, 21, 109)) 1 `shouldBe` [(((33.0,18.0,109.0), (0, 0, 0)), []), (((34.166668,23.5,111.0), (0, 0, 0)), [Pixel (0, 0) (33, 21, 109)])]

    it "linkPixelsToClusters" $ linkPixelsToClusters pixels ([(34.125, 19.5, 109.75), (33, 28.5, 112)], [(33, 21, 112), (33, 25, 112)]) `shouldBe` [
            (((34.125, 19.5, 109.75), (33, 21, 112)), [
                Pixel (0, 0) (33, 18, 109),
                Pixel (0, 1) (33, 18, 109),
                Pixel (0, 2) (33, 21, 109),
                Pixel (0, 3) (33, 21, 112),
                Pixel (1, 0) (33, 18, 109),
                Pixel (1, 1) (35, 18, 109),
                Pixel (1, 2) (35, 21, 109),
                Pixel (1, 3) (38, 21, 112)
            ]),
            (((33, 28.5, 112), (33, 25, 112)), [
                Pixel (0, 4) (33, 25, 112),
                Pixel (0, 5) (33, 32, 112)
            ])
        ]
    it "linkPixelsToClusters" $ linkPixelsToClusters pixels ([(33, 32, 112), (34, 20.11111, 110)], [(33, 32, 112), (35, 21, 109)]) `shouldBe` [
            (((33.0,32.0,112.0), (33.0,32.0,112.0)), [
                Pixel (0, 5) (33, 32, 112)
            ]),
            (((34.0,20.11111,110.0), (35.0,21.0,109.0)),
            [
                Pixel (0, 0) (33, 18, 109),
                Pixel (0, 1) (33, 18, 109),
                Pixel (0, 2) (33, 21, 109),
                Pixel (0, 3) (33, 21, 112),
                Pixel (0, 4) (33, 25, 112),
                Pixel (1, 0) (33, 18, 109),
                Pixel (1, 1) (35, 18, 109),
                Pixel (1, 2) (35, 21, 109),
                Pixel (1, 3) (38, 21, 112)
            ])
        ]

    it "floatSafeDiv" $ floatSafeDiv 2 0 `shouldBe` 0

    it "genMove" $ genMove [] [] `shouldBe` []
    it "genMove" $ genMove [(0, 0, 0)] [] `shouldBe` []
    it "genMove" $ genMove [(33.0,21.11111,113.5), (33.0,32.0,112.0)] [(9, (306, 181, 990)), (1, (33, 32, 112))] `shouldBe` [(0, (1, -1, -3.5)), (1, (0, 0, 0))]

    it "filterMoves" $ filterMoves 0.8 [] `shouldBe` []
    it "filterMoves" $ filterMoves 0.8 [(0, (1, -1, -3.5)), (1, (0, 0, 0))] `shouldBe` [(0, (1, -1, -3.5)), (1, (0, 0, 0))]
    it "filterMoves" $ filterMoves 1.9 [(0, (0, 2.1, 0)),   (1, (0, 2, 0))] `shouldBe` [(0, (0, 2.1, 0)), (1, (0, 2, 0))]
    it "filterMoves" $ filterMoves 2   [(0, (0, 2.1, 0)),   (1, (0, 2, 0))] `shouldBe` [(0, (0, 2.1, 0)), (1, (0, 2, 0))]
    it "filterMoves" $ filterMoves 2.1 [(0, (0, 2.1, 0)),   (1, (0, 2, 0))] `shouldBe` []
    it "filterMoves" $ filterMoves 0.8 [(0, (0, -0.5, -0.1)), (1, (0, 0.6, 0))] `shouldBe` []

    it "findIdx" $ findIdx [] (33, 18, 109) `shouldBe` -1
    it "findIdx" $ findIdx [(34.0, 20.11111, 110.0), (33.0, 32.0, 112.0)] (33, 18, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(34.0, 20.11111, 110.0), (33.0, 32.0, 112.0)] (33, 18, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(34.0, 20.11111, 110.0), (33.0, 32.0, 112.0)] (33, 21, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(34.0, 20.11111, 110.0), (33.0, 32.0, 112.0)] (33, 21, 112) `shouldBe` 0
    it "findIdx" $ findIdx [(34.0, 20.11111, 110.0), (33.0, 32.0, 112.0)] (33, 25, 112) `shouldBe` 0
    it "findIdx" $ findIdx [(34.0, 20.11111, 110.0), (33.0, 32.0, 112.0)] (33, 18, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(34.0, 20.11111, 110.0), (33.0, 32.0, 112.0)] (35, 18, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(34.0, 20.11111, 110.0), (33.0, 32.0, 112.0)] (35, 21, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(34.0, 20.11111, 110.0), (33.0, 32.0, 112.0)] (38, 21, 112) `shouldBe` 0
    it "findIdx" $ findIdx [(34.0, 20.11111, 110.0), (33.0, 32.0, 112.0)] (33, 32, 112) `shouldBe` 1

    it "findIdx" $ findIdx [(34.125, 19.5, 109.75), (33, 28.5, 112)] (33, 18, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(34.125, 19.5, 109.75), (33, 28.5, 112)] (33, 18, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(34.125, 19.5, 109.75), (33, 28.5, 112)] (33, 21, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(34.125, 19.5, 109.75), (33, 28.5, 112)] (33, 21, 112) `shouldBe` 0
    it "findIdx" $ findIdx [(34.125, 19.5, 109.75), (33, 28.5, 112)] (33, 18, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(34.125, 19.5, 109.75), (33, 28.5, 112)] (35, 18, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(34.125, 19.5, 109.75), (33, 28.5, 112)] (35, 21, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(34.125, 19.5, 109.75), (33, 28.5, 112)] (38, 21, 112) `shouldBe` 0
    it "findIdx" $ findIdx [(34.125, 19.5, 109.75), (33, 28.5, 112)] (33, 25, 112) `shouldBe` 1
    it "findIdx" $ findIdx [(34.125, 19.5, 109.75), (33, 28.5, 112)] (33, 32, 112) `shouldBe` 1

    it "findIdx" $ findIdx [(33.0,18.0,109.0), (34.166668,23.5,111.0)] (33, 18, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(33.0,18.0,109.0), (34.166668,23.5,111.0)] (33, 18, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(33.0,18.0,109.0), (34.166668,23.5,111.0)] (33, 18, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(33.0,18.0,109.0), (34.166668,23.5,111.0)] (35, 18, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(33.0,18.0,109.0), (34.166668,23.5,111.0)] (33, 21, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(33.0,18.0,109.0), (34.166668,23.5,111.0)] (33, 21, 112) `shouldBe` 1
    it "findIdx" $ findIdx [(33.0,18.0,109.0), (34.166668,23.5,111.0)] (33, 25, 112) `shouldBe` 1
    it "findIdx" $ findIdx [(33.0,18.0,109.0), (34.166668,23.5,111.0)] (33, 32, 112) `shouldBe` 1
    it "findIdx" $ findIdx [(33.0,18.0,109.0), (34.166668,23.5,111.0)] (35, 21, 109) `shouldBe` 1
    it "findIdx" $ findIdx [(33.0,18.0,109.0), (34.166668,23.5,111.0)] (38, 21, 112) `shouldBe` 1

    it "findIdx" $ findIdx [(33.57143,19.285715,109.42857), (33.0,28.5,112.0)] (33, 18, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(33.57143,19.285715,109.42857), (33.0,28.5,112.0)] (33, 18, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(33.57143,19.285715,109.42857), (33.0,28.5,112.0)] (33, 21, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(33.57143,19.285715,109.42857), (33.0,28.5,112.0)] (33, 21, 112) `shouldBe` 0
    it "findIdx" $ findIdx [(33.57143,19.285715,109.42857), (33.0,28.5,112.0)] (33, 18, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(33.57143,19.285715,109.42857), (33.0,28.5,112.0)] (35, 18, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(33.57143,19.285715,109.42857), (33.0,28.5,112.0)] (35, 21, 109) `shouldBe` 0
    it "findIdx" $ findIdx [(33.57143,19.285715,109.42857), (33.0,28.5,112.0)] (38, 21, 112) `shouldBe` 0
    it "findIdx" $ findIdx [(33.57143,19.285715,109.42857), (33.0,28.5,112.0)] (33, 25, 112) `shouldBe` 1
    it "findIdx" $ findIdx [(33.57143,19.285715,109.42857), (33.0,28.5,112.0)] (33, 32, 112) `shouldBe` 1

    it "findIdx" $ findIdx [(4, 4, 0), (4, 4, 8), (4, 4, 4), (4, 4, 12)] (8, 4, -2) `shouldBe` 0
    it "findIdx" $ findIdx [(4, 4, 0), (4, 4, 8), (4, 4, 4), (4, 4, 12)] (8, 4, 2)  `shouldBe` 0
    it "findIdx" $ findIdx [(4, 4, 0), (4, 4, 8), (4, 4, 4), (4, 4, 12)] (8, 4, 6)  `shouldBe` 1
    it "findIdx" $ findIdx [(4, 4, 0), (4, 4, 8), (4, 4, 4), (4, 4, 12)] (8, 4, 7)  `shouldBe` 1
    it "findIdx" $ findIdx [(4, 4, 0), (4, 4, 8), (4, 4, 4), (4, 4, 12)] (8, 4, 10) `shouldBe` 1
    it "findIdx" $ findIdx [(4, 4, 0), (4, 4, 8), (4, 4, 4), (4, 4, 12)] (8, 4, 3)  `shouldBe` 2
    it "findIdx" $ findIdx [(4, 4, 0), (4, 4, 8), (4, 4, 4), (4, 4, 12)] (8, 4, 11) `shouldBe` 3
    it "findIdx" $ findIdx [(4, 4, 0), (4, 4, 8), (4, 4, 4), (4, 4, 12)] (8, 4, 17) `shouldBe` 3

    it "insertCol" $ insertCol [] (0, 0, 0) 0 `shouldBe` []
    it "insertCol" $ insertCol [(9, (306, 181, 990)), (1, (33, 32, 112))] (0, 0, 0) 0 `shouldBe` [(10, (306, 181, 990)), (1, (33, 32, 112))]
    it "insertCol" $ insertCol [(9, (306, 181, 990)), (1, (33, 32, 112))] (33, 244, 109) 0 `shouldBe` [(10, (306+33, 181+244, 990+109)), (1, (33, 32, 112))]
    it "insertCol" $ insertCol [(9, (306, 181, 990)), (1, (33, 32, 112))] (33, 244, 109) 1 `shouldBe` [(9, (306, 181, 990)), (2, (33+33, 32+244, 112+109))]

    it "generateEmptyTotals" $ generateEmptyTotals 0 `shouldBe` []
    it "generateEmptyTotals" $ generateEmptyTotals 1 `shouldBe` [(0, (0, 0, 0))]
    it "generateEmptyTotals" $ generateEmptyTotals 2 `shouldBe` [(0, (0, 0, 0)), (0, (0, 0, 0))]
    it "generateEmptyTotals" $ generateEmptyTotals 4 `shouldBe` [(0, (0, 0, 0)), (0, (0, 0, 0)), (0, (0, 0, 0)), (0, (0, 0, 0))]

    it "appMove" $ appMove [(33.0,18.0,109.0), (34.166668,23.5,111.0)] [] `shouldBe` [(33.0,18.0,109.0), (34.166668,23.5,111.0)]
    it "appMove" $ appMove [(33.0,18.0,109.0), (34.166668,23.5,111.0)] [(1, (1, -1, 4))] `shouldBe` [(33.0,18.0,109.0), (34.166668+1,23.5-1,111.0+4)]
    it "appMove" $ appMove [(33.0,18.0,109.0), (34.166668,23.5,111.0)] [(0, (1.5, -10, 4.12)), (1, (1, -1, 4))] `shouldBe` [(33.0+1.5,18.0-10,109.0+4.12), (34.166668+1,23.5-1,111.0+4)]
    it "appMove" $ appMove [(33.0,18.0,109.0), (1, 2, 3), (34.166668,23.5,111.0)] [(0, (1.5, -10, 4.12)), (2, (1, -1, 4))] `shouldBe` [(33.0+1.5,18.0-10,109.0+4.12), (1, 2, 3), (34.166668+1,23.5-1,111.0+4)]

    it "generateClusters" $ generateClusters 0.8 pixels [(33, 18, 109), (35, 18, 109)] `shouldBe` ([(33.0,28.5,112.0), (34.125,19.5,109.75)], [(33.0,24.75,111.25), (34.5,19.0,109.5)])
    it "generateClusters" $ generateClusters 0.8 pixels [(33, 32, 112), (35, 21, 109)] `shouldBe` ([(33, 32, 112), (34, 20.11111, 110)], [(33, 32, 112), (35, 21, 109)])
    it "generateClusters" $ generateClusters 0.8 pixels [(33, 32, 112), (34, 20.11111, 110)] `shouldBe` ([(33, 32, 112), (34, 20.11111, 110)], [(33, 32, 112), (34, 20.11111, 110)])

    it "compress" $ compress seed (CompressorConf 0 0.8 pixels) `shouldBe` []
    it "compress" $ compress seed (CompressorConf 2 0.8 []    ) `shouldBe` []
    it "compress" $ compress seed (CompressorConf 2 0.8 pixels) `shouldBe` [
            (((33.5, 18, 109), (35, 18, 109)), [
                Pixel (0, 0) (33, 18, 109),
                Pixel (0, 1) (33, 18, 109),
                Pixel (1, 0) (33, 18, 109),
                Pixel (1, 1) (35, 18, 109)
            ]),
            (((34.166668, 23.5, 111), (35, 21, 109)), [
                Pixel (0, 2) (33, 21, 109),
                Pixel (0, 3) (33, 21, 112),
                Pixel (0, 4) (33, 25, 112),
                Pixel (0, 5) (33, 32, 112),
                Pixel (1, 2) (35, 21, 109),
                Pixel (1, 3) (38, 21, 112)
            ])
        ]
