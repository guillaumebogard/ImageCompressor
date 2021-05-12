--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- ClusterSpec
--

module ClusterSpec ( spec ) where

import Test.Hspec  ( Spec
                   , it
                   , shouldBe
                   , shouldThrow
                   , anyException )

import Vector      ( Vector3(Vector3) )
import Cluster     ( findClosestCluster, generateMoves)

spec :: Spec
spec = do
    it "generateMoves" $ generateMoves [] [] `shouldBe` []
    it "generateMoves" $ generateMoves [Vector3 0    0        0] [] `shouldBe` []
    it "generateMoves" $ generateMoves [Vector3 33.0 21.11111 113.5, Vector3 33.0 32.0 112.0] [(9, Vector3 306 181 990), (1, Vector3 33 32 112)] `shouldBe` [Vector3 1 (-1) (-3.5), Vector3 0 0 0]

    it "findClosestCluster" $ findClosestCluster []                                                     (Vector3 33 18 109) `shouldBe` -1
    it "findClosestCluster" $ findClosestCluster [Vector3 34.0 20.11111 110.0, Vector3 33.0 32.0 112.0] (Vector3 33 18 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 34.0 20.11111 110.0, Vector3 33.0 32.0 112.0] (Vector3 33 18 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 34.0 20.11111 110.0, Vector3 33.0 32.0 112.0] (Vector3 33 21 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 34.0 20.11111 110.0, Vector3 33.0 32.0 112.0] (Vector3 33 21 112) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 34.0 20.11111 110.0, Vector3 33.0 32.0 112.0] (Vector3 33 25 112) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 34.0 20.11111 110.0, Vector3 33.0 32.0 112.0] (Vector3 33 18 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 34.0 20.11111 110.0, Vector3 33.0 32.0 112.0] (Vector3 35 18 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 34.0 20.11111 110.0, Vector3 33.0 32.0 112.0] (Vector3 35 21 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 34.0 20.11111 110.0, Vector3 33.0 32.0 112.0] (Vector3 38 21 112) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 34.0 20.11111 110.0, Vector3 33.0 32.0 112.0] (Vector3 33 32 112) `shouldBe` 1

    it "findClosestCluster" $ findClosestCluster [Vector3 34.125 19.5 109.75, Vector3 33 28.5 112] (Vector3 33 18 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 34.125 19.5 109.75, Vector3 33 28.5 112] (Vector3 33 18 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 34.125 19.5 109.75, Vector3 33 28.5 112] (Vector3 33 21 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 34.125 19.5 109.75, Vector3 33 28.5 112] (Vector3 33 21 112) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 34.125 19.5 109.75, Vector3 33 28.5 112] (Vector3 33 18 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 34.125 19.5 109.75, Vector3 33 28.5 112] (Vector3 35 18 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 34.125 19.5 109.75, Vector3 33 28.5 112] (Vector3 35 21 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 34.125 19.5 109.75, Vector3 33 28.5 112] (Vector3 38 21 112) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 34.125 19.5 109.75, Vector3 33 28.5 112] (Vector3 33 25 112) `shouldBe` 1
    it "findClosestCluster" $ findClosestCluster [Vector3 34.125 19.5 109.75, Vector3 33 28.5 112] (Vector3 33 32 112) `shouldBe` 1

    it "findClosestCluster" $ findClosestCluster [Vector3 33.0 18.0 109.0, Vector3 34.166668 23.5 111.0] (Vector3 33 18 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 33.0 18.0 109.0, Vector3 34.166668 23.5 111.0] (Vector3 33 18 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 33.0 18.0 109.0, Vector3 34.166668 23.5 111.0] (Vector3 33 18 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 33.0 18.0 109.0, Vector3 34.166668 23.5 111.0] (Vector3 35 18 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 33.0 18.0 109.0, Vector3 34.166668 23.5 111.0] (Vector3 33 21 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 33.0 18.0 109.0, Vector3 34.166668 23.5 111.0] (Vector3 33 21 112) `shouldBe` 1
    it "findClosestCluster" $ findClosestCluster [Vector3 33.0 18.0 109.0, Vector3 34.166668 23.5 111.0] (Vector3 33 25 112) `shouldBe` 1
    it "findClosestCluster" $ findClosestCluster [Vector3 33.0 18.0 109.0, Vector3 34.166668 23.5 111.0] (Vector3 33 32 112) `shouldBe` 1
    it "findClosestCluster" $ findClosestCluster [Vector3 33.0 18.0 109.0, Vector3 34.166668 23.5 111.0] (Vector3 35 21 109) `shouldBe` 1
    it "findClosestCluster" $ findClosestCluster [Vector3 33.0 18.0 109.0, Vector3 34.166668 23.5 111.0] (Vector3 38 21 112) `shouldBe` 1

    it "findClosestCluster" $ findClosestCluster [Vector3 33.57143 19.285715 109.42857, Vector3 33.0 28.5 112.0] (Vector3 33 18 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 33.57143 19.285715 109.42857, Vector3 33.0 28.5 112.0] (Vector3 33 18 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 33.57143 19.285715 109.42857, Vector3 33.0 28.5 112.0] (Vector3 33 21 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 33.57143 19.285715 109.42857, Vector3 33.0 28.5 112.0] (Vector3 33 21 112) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 33.57143 19.285715 109.42857, Vector3 33.0 28.5 112.0] (Vector3 33 18 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 33.57143 19.285715 109.42857, Vector3 33.0 28.5 112.0] (Vector3 35 18 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 33.57143 19.285715 109.42857, Vector3 33.0 28.5 112.0] (Vector3 35 21 109) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 33.57143 19.285715 109.42857, Vector3 33.0 28.5 112.0] (Vector3 38 21 112) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 33.57143 19.285715 109.42857, Vector3 33.0 28.5 112.0] (Vector3 33 25 112) `shouldBe` 1
    it "findClosestCluster" $ findClosestCluster [Vector3 33.57143 19.285715 109.42857, Vector3 33.0 28.5 112.0] (Vector3 33 32 112) `shouldBe` 1

    it "findClosestCluster" $ findClosestCluster [Vector3 4 4 0, Vector3 4 4 8, Vector3 4 4 4, Vector3 4 4 12] (Vector3 8 4 (-2)) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 4 4 0, Vector3 4 4 8, Vector3 4 4 4, Vector3 4 4 12] (Vector3 8 4 2   ) `shouldBe` 0
    it "findClosestCluster" $ findClosestCluster [Vector3 4 4 0, Vector3 4 4 8, Vector3 4 4 4, Vector3 4 4 12] (Vector3 8 4 6   ) `shouldBe` 1
    it "findClosestCluster" $ findClosestCluster [Vector3 4 4 0, Vector3 4 4 8, Vector3 4 4 4, Vector3 4 4 12] (Vector3 8 4 7   ) `shouldBe` 1
    it "findClosestCluster" $ findClosestCluster [Vector3 4 4 0, Vector3 4 4 8, Vector3 4 4 4, Vector3 4 4 12] (Vector3 8 4 10  ) `shouldBe` 1
    it "findClosestCluster" $ findClosestCluster [Vector3 4 4 0, Vector3 4 4 8, Vector3 4 4 4, Vector3 4 4 12] (Vector3 8 4 3   ) `shouldBe` 2
    it "findClosestCluster" $ findClosestCluster [Vector3 4 4 0, Vector3 4 4 8, Vector3 4 4 4, Vector3 4 4 12] (Vector3 8 4 11  ) `shouldBe` 3
    it "findClosestCluster" $ findClosestCluster [Vector3 4 4 0, Vector3 4 4 8, Vector3 4 4 4, Vector3 4 4 12] (Vector3 8 4 17  ) `shouldBe` 3
