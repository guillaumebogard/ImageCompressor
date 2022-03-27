module Argument.Parser.Exception ( ArgumentParserException(..) ) where

import Control.Exception         ( Exception )


data ArgumentParserException = ArgumentParserHelpException
                             | ArgumentParserException     String

instance Exception ArgumentParserException

instance Show      ArgumentParserException where
    show ArgumentParserHelpException            = usage
    show (ArgumentParserException    exception) = "Argument Parser Exception: " ++ exception ++ "."

usage :: String
usage = "USAGE: ./imageCompressor -n N -l L -f F\n"                   ++
        "\n"                                                          ++
        "\tN\tnumber of colors in the final image\n"                  ++
        "\tL\tconvergence limit\n"                                    ++
        "\tF\tpath to the file containing the colors of the pixels\n"
