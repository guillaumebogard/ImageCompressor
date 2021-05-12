--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- CreatePixelSpec
--

module FileParsingSpec ( spec ) where

import Test.Hspec               ( Spec
                                , it
                                , shouldBe
                                , shouldThrow
                                , anyException )

import Control.Exception        ( evaluate )

import Errors                   ( CompressorError(FileParse, FileParseColorError) )
import FileParsing.Pixel        ( Pixel(Pixel) )
import FileParsing.CreatePixel  ( createPixel )
import Vector                   ( Vector3(Vector3), Vector2(Vector2) )

spec :: Spec
spec = do
    it "createPixel" $ createPixel "(0,1) (1,1,1)"             `shouldBe`    Pixel (Vector2 0 1) (Vector3 1 1 1)
    it "createPixel" $ evaluate(createPixel "(?,?) (?,?,?)")   `shouldThrow` (== FileParse)
    it "createPixel" $ evaluate(createPixel "(1,2) (1,1,?)")   `shouldThrow` (== FileParse)
    it "createPixel" $ evaluate(createPixel "")                `shouldThrow` (== FileParse)
    it "createPixel" $ evaluate(createPixel "(")               `shouldThrow` (== FileParse)
    it "createPixel" $ evaluate(createPixel "() ()")           `shouldThrow` (== FileParse)
    it "createPixel" $ evaluate(createPixel "(0,1) (1,256,0)") `shouldThrow` (== FileParseColorError)
    it "createPixel" $ evaluate(createPixel "(0,1) (1,-1,0)")  `shouldThrow` (== FileParseColorError)
