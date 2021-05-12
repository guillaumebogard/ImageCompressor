--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- RandomManagerSpec
--

module RandomManagerSpec ( spec ) where

import Test.Hspec       ( Spec
                        , it
                        , shouldBe
                        , shouldThrow
                        , anyException )

import System.Random    ( mkStdGen )

import RandomManager    ( makeNbRandomsUnique )

spec :: Spec
spec = do
    let seed = mkStdGen 230844504580

    it "makeNbRandomsUnique" $ makeNbRandomsUnique seed 0 9   `shouldBe` []
    it "makeNbRandomsUnique" $ makeNbRandomsUnique seed 2 9   `shouldBe` [8, 7]
    it "makeNbRandomsUnique" $ makeNbRandomsUnique seed 4 100 `shouldBe` [52, 6, 90, 68]
    it "makeNbRandomsUnique" $ makeNbRandomsUnique seed 2 2   `shouldBe` [0, 2]
    it "makeNbRandomsUnique" $ makeNbRandomsUnique seed 7 7   `shouldBe` [2, 1, 6, 7, 0, 5, 4]
