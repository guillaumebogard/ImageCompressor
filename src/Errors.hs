--
-- EPITECH PROJECT, 2021
-- B-FUN-400-BDX-4-1-compressor-guillaume.bogard-coquard
-- File description:
-- Errors
--

module Errors ( CompressorError (..) ) where

import Control.Exception ( Exception )

data CompressorError = ArgumentError String | FileParse | FileParseColorError
                deriving (Show, Eq)

instance Exception CompressorError
