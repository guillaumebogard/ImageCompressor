--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- CreatePixel
--

module CreatePixel ( Pixel(..)
                    , createPixel
                    ) where

import System.IO.Unsafe ( unsafePerformIO )
import Control.Exception    ( throw )
import Errors               ( MyError(..) )

type PosX   = Int
type PosY   = Int
type ColorR = Int
type ColorG = Int
type ColorB = Int

data Pixel = Pixel (PosX, PosY) (ColorR, ColorG, ColorB)
instance Show Pixel where
    show (Pixel (x, y) (r, g, b)) = "(" ++ show x ++ ", " ++ show y ++ ") (" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"

data Token = OPEN_PARENTHESIS | CLOSE_PARENTHESIS | COMMA | SPACE | Number String
instance Show Token where
    show OPEN_PARENTHESIS = "OPEN_PARENTHESIS"
    show CLOSE_PARENTHESIS = "CLOSE_PARENTHESIS"
    show COMMA = "COMMA"
    show SPACE = "SPACE"
    show (Number v) = "Number: " ++ v

createPixel :: String -> Pixel
createPixel str = parsing $ tokenize ([], str)

tokenize :: ([Token], String) -> [Token]
tokenize (t, []) = t
tokenize (t, '(':xs)         = do
                                _ <- logVar "("
                                OPEN_PARENTHESIS   : tokenize (t, xs)
tokenize (t, ')':xs)         = do
                                _ <- logVar ")"
                                CLOSE_PARENTHESIS  : tokenize (t, xs)
tokenize (t, ' ':xs)         = do
                                _ <- logVar " "
                                SPACE              : tokenize (t, xs)
tokenize (t, ',':xs)         = do
                                _ <- logVar ","
                                COMMA              : tokenize (t, xs)
-- tokenize (Number n:ts, x:xs) = do
--                                 _ <- logVar [x]
--                                 -- _ <- logVar xs
--                                 Number (x : n)
tokenize (t, x:xs)           = do
                                _ <- logVar ['N', x]
                                Number [x]         : tokenize (t, xs)
tokenize (_, x:xs) = do
                    _ <- logVar "FUUUUUUCCKK"
                    throw $ InputError "Yes"
tokenize _ = do
                    _ <- logVar "SSSHHHHIIIIIITTTT"
                    throw $ InputError "Fuck"

logIO :: Show a => a -> IO a
logIO var = do putStr $ (show var) ++ " "
               return var

logVar :: Show a => a -> a
logVar var = unsafePerformIO $ logIO var

parsing :: [Token] -> Pixel
parsing [] = Pixel (0, 0) (0, 0, 0)
-- parsing (Number v:ts) = do
--                         _ <- logVar v
--                         parsing ts
parsing (t:ts) = parsing ts

-- parsing [OPEN_PARENTHESIS, (Number x), COMMA, (Number y), CLOSE_PARENTHESIS, SPACE, OPEN_PARENTHESIS, (Number r), COMMA, (Number g), COMMA, (Number b), CLOSE_PARENTHESIS]
--         = Pixel (read x, read y) (read r, read g, read b)
-- paring [] = Pixel (0, 0) (0, 0, 0)
-- parsing (t:ts) = do
--             _ <- logVar $ show t
--             parsing ts

