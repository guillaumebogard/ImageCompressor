module LibSpec (spec) where

import Test.Hspec   ( Spec
                    , it
                    , shouldBe
                    , shouldThrow
                    , anyException)

import Control.Exception (evaluate)

import Error
import Lexing

import Parsing

import Rules

import Wolfram

spec :: Spec
spec = do
-- Rule 30
    it "rule30-case1" $ rule30 '*' '*' '*' `shouldBe` ' '
    it "rule30-case2" $ rule30 '*' '*' ' ' `shouldBe` ' '
    it "rule30-case3" $ rule30 '*' ' ' '*' `shouldBe` ' '
    it "rule30-case4" $ rule30 '*' ' ' ' ' `shouldBe` '*'
    it "rule30-case5" $ rule30 ' ' '*' '*' `shouldBe` '*'
    it "rule30-case6" $ rule30 ' ' '*' ' ' `shouldBe` '*'
    it "rule30-case7" $ rule30 ' ' ' ' '*' `shouldBe` '*'
    it "rule30-case8" $ rule30 ' ' ' ' ' ' `shouldBe` ' '

-- Rule 90
    it "rule90-case1" $ rule90 '*' '*' '*' `shouldBe` ' '
    it "rule90-case2" $ rule90 '*' '*' ' ' `shouldBe` '*'
    it "rule90-case3" $ rule90 '*' ' ' '*' `shouldBe` ' '
    it "rule90-case4" $ rule90 '*' ' ' ' ' `shouldBe` '*'
    it "rule90-case5" $ rule90 ' ' '*' '*' `shouldBe` '*'
    it "rule90-case6" $ rule90 ' ' '*' ' ' `shouldBe` ' '
    it "rule90-case7" $ rule90 ' ' ' ' '*' `shouldBe` '*'
    it "rule90-case8" $ rule90 ' ' ' ' ' ' `shouldBe` ' '

-- Rule 110
    it "rule110-case1" $ rule110 '*' '*' '*' `shouldBe` ' '
    it "rule110-case2" $ rule110 '*' '*' ' ' `shouldBe` '*'
    it "rule110-case3" $ rule110 '*' ' ' '*' `shouldBe` '*'
    it "rule110-case4" $ rule110 '*' ' ' ' ' `shouldBe` ' '
    it "rule110-case5" $ rule110 ' ' '*' '*' `shouldBe` '*'
    it "rule110-case6" $ rule110 ' ' '*' ' ' `shouldBe` '*'
    it "rule110-case7" $ rule110 ' ' ' ' '*' `shouldBe` '*'
    it "rule110-case8" $ rule110 ' ' ' ' ' ' `shouldBe` ' '

-- Tokenize
    it "tokenize-rule_arg"            $ tokenize ["--rule", "30"]                  `shouldBe` [RULE, Value "30"]
    it "tokenize-rule_and_lines_args" $ tokenize ["--rule", "30", "--lines", "10"] `shouldBe` [RULE, Value "30", LINES, Value "10"]
    it "tokenize-lines_invalid_arg"   $ tokenize ["--lines", "a"]                  `shouldBe` [LINES, Value "a"]

-- Parsing
    it "parsing-default_conf" $ parsing defaultConf []                  `shouldBe` defaultConf
    it "parsing-set_rule"     $ parsing defaultConf [RULE, Value "30"]  `shouldBe` ParsingConf (Just 30) 0 Nothing   80 0
    it "parsing-set_start"    $ parsing defaultConf [START, Value "50"] `shouldBe` ParsingConf Nothing  50 Nothing   80 0
    it "parsing-set_lines"    $ parsing defaultConf [LINES, Value "10"] `shouldBe` ParsingConf Nothing  0  (Just 10) 80 0
    it "parsing-set_window"   $ parsing defaultConf [WINDOW, Value "5"] `shouldBe` ParsingConf Nothing  0  Nothing   5  0
    it "parsing-set_move"     $ parsing defaultConf [MOVE , Value "20"] `shouldBe` ParsingConf Nothing  0  Nothing   80 30
    it "parsing-set_invalid_rule"  $ evaluate (parsing defaultConf [RULE,  Value "a"]) `shouldThrow` anyException
    it "parsing-set_invalid_lines" $ evaluate (parsing defaultConf [LINES, Value "a"]) `shouldThrow` anyException
