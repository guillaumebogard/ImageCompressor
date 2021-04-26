module Errors (
    MyError (..)
) where

import Control.Exception ( Exception )

data MyError = InputError String | OtherError String
                deriving (Show, Eq)

instance Exception MyError
