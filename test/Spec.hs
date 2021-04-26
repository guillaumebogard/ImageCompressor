module Main (main) where

import LibSpec (spec)

import Test.Hspec   ( hspec
                    , Spec
                    , describe)

spec :: Spec
spec = do
    describe "Lib" LibSpec.spec

main :: IO ()
main = hspec Main.spec
