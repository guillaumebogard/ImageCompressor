module           Main                       ( main ) where

import qualified Test.Hspec           as TH ( Spec
                                            , hspec
                                            , describe )

import qualified Argument.Lexer.Spec as ALS ( spec )

main :: IO ()
main = TH.hspec Main.spec

spec :: TH.Spec
spec = do
    TH.describe "Argument.Lexer" ALS.spec
