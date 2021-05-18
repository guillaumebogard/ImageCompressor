--
-- EPITECH PROJECT, 2021
-- B-FUN-400-BDX-4-1-compressor-guillaume.bogard-coquard
-- File description:
-- Vector
--

{-# LANGUAGE GADTs #-}

module Vector ( Vector2(..)
              , Vector3(..)
              , getDistanceVector2
              , getDistanceVector3
              , getLengthVector2
              , getLengthVector3
              , vector2itn
              , vector3itn
              , vector2fti
              , vector3fti
              ) where

data Vector2 a where Vector2 :: Num a => a -> a -> Vector2 a
instance Show a => Show (Vector2 a) where
    show (Vector2 x y) = '(' : show x ++ ',' : show y ++ ")"
instance Eq a => Eq (Vector2 a) where
    (==) (Vector2 x1 y1) (Vector2 x2 y2) = x1 == x2 && y1 == y2

-- data Vector3 a b c where Vector3 :: (Num a, Num b, Num c) => a -> b -> c -> Vector3 a b c
-- instance (Show a, Show b, Show c) => Show (Vector3 a b c) where
data Vector3 a where Vector3 :: Num a => a -> a -> a -> Vector3 a
instance Show a => Show (Vector3 a) where
    show (Vector3 x y z)    = '(' : show x ++ ',' : show y ++ ',' : show z ++ ")"
instance Eq a => Eq (Vector3 a) where
    (==) (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2

getDistanceVector2 :: Floating a => Vector2 a -> Vector2 a -> a
getDistanceVector2 (Vector2 x1 y1) (Vector2 x2 y2) = sqrt ((x1 - x2) ^ (2 :: Integer) + (y1 - y2) ^ (2 :: Integer))

-- getDistanceVector3 :: (Floating a, Floating b, Floating c) => Vector3 a b c -> Vector3 a b c -> Distance d
getDistanceVector3 :: Floating a => Vector3 a -> Vector3 a -> a
getDistanceVector3 (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = sqrt ((x1 - x2) ^ (2 :: Integer) + (y1 - y2) ^ (2 :: Integer) + (z1 - z2) ^ (2 :: Integer))

getLengthVector2 :: Floating a => Vector2 a -> a
getLengthVector2 (Vector2 x y) = sqrt (x ^ (2 :: Integer) + y ^ (2 :: Integer))

getLengthVector3 :: Floating a => Vector3 a -> a
getLengthVector3 (Vector3 x y z) = sqrt (x ^ (2 :: Integer) + y ^ (2 :: Integer) + z ^ (2 :: Integer))

vector2itn :: (Integral a, Num b) => Vector2 a -> Vector2 b
vector2itn (Vector2 x y) = Vector2 (fromIntegral x) (fromIntegral y)

vector3itn :: (Integral a, Num b) => Vector3 a -> Vector3 b
vector3itn (Vector3 x y z) = Vector3 (fromIntegral x) (fromIntegral y) (fromIntegral z)

vector2fti :: (RealFrac a) => Vector2 a -> Vector2 Integer
vector2fti (Vector2 x y) = Vector2 (round x) (round y)

vector3fti :: (RealFrac a) => Vector3 a -> Vector3 Integer
vector3fti (Vector3 x y z) = Vector3 (round x) (round y) (round z)
