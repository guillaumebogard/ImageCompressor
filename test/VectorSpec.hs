--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- VectorSpec
--

module VectorSpec ( spec ) where

import Test.Hspec ( Spec
                  , it
                  , shouldBe
                  , shouldThrow
                  , anyException )

import Vector     ( Vector3(Vector3)
                  , Vector2(Vector2)
                  , getDistanceVector2
                  , getDistanceVector3
                  , getLengthVector2
                  , getLengthVector3
                  , vector2fti
                  , vector2itn
                  , vector3fti
                  , vector3itn )

spec :: Spec
spec = do
    it "getDistanceVector2" $ getDistanceVector2 (Vector2 1        2       ) (Vector2 1       2         ) `shouldBe` 0
    it "getDistanceVector2" $ getDistanceVector2 (Vector2 1        2       ) (Vector2 1       3         ) `shouldBe` 1
    it "getDistanceVector2" $ getDistanceVector2 (Vector2 (-30621) 49165   ) (Vector2 16163   (-9501394)) `shouldBe` 9550673.58646169
    it "getDistanceVector2" $ getDistanceVector2 (Vector2 76205438 52819151) (Vector2 2126727 22741779  ) `shouldBe` 79951883.84177014

    it "getDistanceVector3" $ getDistanceVector3 (Vector3 1        2        3       ) (Vector3 1       2          3          ) `shouldBe` 0
    it "getDistanceVector3" $ getDistanceVector3 (Vector3 1        2        3       ) (Vector3 1       2          4          ) `shouldBe` 1
    it "getDistanceVector3" $ getDistanceVector3 (Vector3 (-30621) 49165    35220   ) (Vector3 16163   (-9501394) (-15668454)) `shouldBe` 18379900.518049955
    it "getDistanceVector3" $ getDistanceVector3 (Vector3 76205438 52819151 92800455) (Vector3 2126727 22741779   (-85538615)) `shouldBe` 195440854.52717608

    it "getLengthVector2" $ getLengthVector2 (Vector2 0        0       ) `shouldBe` 0
    it "getLengthVector2" $ getLengthVector2 (Vector2 1        1       ) `shouldBe` 1.4142135623730951
    it "getLengthVector2" $ getLengthVector2 (Vector2 (-30621) 49165   ) `shouldBe` 57921.00539527953
    it "getLengthVector2" $ getLengthVector2 (Vector2 76205438 52819151) `shouldBe` 92720717.7125622

    it "getLengthVector3" $ getLengthVector3 (Vector3 0        0        0       ) `shouldBe` 0
    it "getLengthVector3" $ getLengthVector3 (Vector3 1        1        1       ) `shouldBe` 1.7320508075688772
    it "getLengthVector3" $ getLengthVector3 (Vector3 (-30621) 49165    35220   ) `shouldBe` 67788.57769565607
    it "getLengthVector3" $ getLengthVector3 (Vector3 76205438 52819151 92800455) `shouldBe` 131183291.39543523

    it "vector2itn" $ vector2itn (Vector2 0        0       ) `shouldBe` Vector2 0.0        0.0
    it "vector2itn" $ vector2itn (Vector2 1        1       ) `shouldBe` Vector2 1.0        1.0
    it "vector2itn" $ vector2itn (Vector2 (-30621) 49165   ) `shouldBe` Vector2 (-30621.0) 49165.0
    it "vector2itn" $ vector2itn (Vector2 76205438 52819151) `shouldBe` Vector2 76205438.0 52819151.0

    it "vector3itn" $ vector3itn (Vector3 0        0        0       ) `shouldBe` Vector3 0.0        0.0        0.0
    it "vector3itn" $ vector3itn (Vector3 1        1        1       ) `shouldBe` Vector3 1.0        1.0        1.0
    it "vector3itn" $ vector3itn (Vector3 (-30621) 49165    35220   ) `shouldBe` Vector3 (-30621.0) 49165.0    35220.0
    it "vector3itn" $ vector3itn (Vector3 76205438 52819151 92800455) `shouldBe` Vector3 76205438.0 52819151.0 92800455.0

    it "vector2fti" $ vector2fti (Vector2 0.0        0.0       ) `shouldBe` Vector2 0        0
    it "vector2fti" $ vector2fti (Vector2 1.0        1.0       ) `shouldBe` Vector2 1        1
    it "vector2fti" $ vector2fti (Vector2 (-30621.0) 49165.0   ) `shouldBe` Vector2 (-30621) 49165
    it "vector2fti" $ vector2fti (Vector2 76205438.0 52819151.0) `shouldBe` Vector2 76205438 52819151

    it "vector3fti" $ vector3fti (Vector3 0.0        0.0        0.0       ) `shouldBe` Vector3 0        0        0
    it "vector3fti" $ vector3fti (Vector3 1.0        1.0        1.0       ) `shouldBe` Vector3 1        1        1
    it "vector3fti" $ vector3fti (Vector3 (-30621.0) 49165.0    35220.0   ) `shouldBe` Vector3 (-30621) 49165    35220
    it "vector3fti" $ vector3fti (Vector3 76205438.0 52819151.0 92800455.0) `shouldBe` Vector3 76205438 52819151 92800455
