--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- Usage
--

module Usage ( printUsage ) where

usage :: String
usage = "USAGE: ./imageCompressor -n N -l L -f F\n\n\tN\tnumber of colors in the final image\n\tL\tconvergence limit\n\tF\tpath to the file containing the colors of the pixels"

printUsage :: IO ()
printUsage = putStrLn usage
