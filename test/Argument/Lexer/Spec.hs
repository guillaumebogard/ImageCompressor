module           Argument.Lexer.Spec       ( spec ) where

import qualified Test.Hspec          as TH ( Spec
                                           , it )

import qualified Argument.Lexer      as AL ( Token(..)
                                           , tokenize )


newtype TestToken = TestToken AL.Token


instance Eq TestToken where
    (TestToken AL.NbColorsOption      ) == (TestToken AL.NbColorsOption       ) = True
    (TestToken AL.LimitOption         ) == (TestToken AL.LimitOption          ) = True
    (TestToken AL.FilepathOption      ) == (TestToken AL.FilepathOption       ) = True
    (TestToken (AL.Argument      left)) == (TestToken (AL.Argument      right)) = left == right
    (TestToken AL.HelpOption          ) == (TestToken AL.HelpOption           ) = True
    (TestToken (AL.UnknownOption left)) == (TestToken (AL.UnknownOption right)) = left == right
    _                                   == _                                    = False

spec :: TH.Spec
spec = do
    TH.it "Single short help option" $
        map TestToken (AL.tokenize ["-h"])
            == map TestToken [
                AL.HelpOption
            ]
    TH.it "Single long help option" $
        map TestToken (AL.tokenize ["--help"])
            == map TestToken [
                AL.HelpOption
            ]
    TH.it "Multiple short & long help options" $
        map TestToken (AL.tokenize ["-h", "--help"])
            == map TestToken [
                AL.HelpOption
              , AL.HelpOption
            ]
    TH.it "NbColors, Limit, Filepath options" $
        map TestToken (AL.tokenize ["-n", "10", "-l", "0.8", "-f", "pixels.data"])
            == map TestToken [
                AL.NbColorsOption
              , AL.Argument "10"
              , AL.LimitOption
              , AL.Argument "0.8"
              , AL.FilepathOption
              , AL.Argument "pixels.data"
            ]
    TH.it "Unknown option given" $
        map TestToken (AL.tokenize ["-p"])
            == map TestToken [
                AL.UnknownOption "-p"
            ]
