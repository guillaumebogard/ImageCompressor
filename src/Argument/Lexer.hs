module Argument.Lexer ( Token(..)
                      , tokenize
                      ) where

data Token = NbColorsOption
           | LimitOption
           | FilepathOption
           | Argument       String
           | HelpOption
           | UnknownOption  String


tokenize :: [String] -> [Token]
tokenize []               = []
tokenize ("-h"       :xs) = HelpOption         : tokenize xs
tokenize ("--help"   :xs) = HelpOption         : tokenize xs
tokenize ("-n"       :xs) = NbColorsOption     : tokenize xs
tokenize ("-l"       :xs) = LimitOption        : tokenize xs
tokenize ("-f"       :xs) = FilepathOption     : tokenize xs
tokenize (opt@('-':_):xs) = UnknownOption  opt : tokenize xs
tokenize (x          :xs) = Argument       x   : tokenize xs
