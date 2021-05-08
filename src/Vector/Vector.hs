--
-- EPITECH PROJECT, 2021
-- B-FUN-400-BDX-4-1-compressor-guillaume.bogard-coquard
-- File description:
-- Vector
--

module Vector.Vector where

newtype Vector2 a = Vector2 (a, a)
instance Show a => Show (Vector2 a) where
    show (Vector2 (x, y))    = '(' : show x ++ ',' : show y ++ ")"
instance Eq a => Eq (Vector2 a) where
    (==) (Vector2 (x1, y1))     (Vector2 (x2, y2))     = x1 == x2 && y1 == y2

newtype Vector3 a = Vector3 (a, a, a)
instance Show a => Show (Vector3 a) where
    show (Vector3 (x, y, z)) = '(' : show x ++ ',' : show y ++ ',' : show z ++ ")"
instance Eq a => Eq (Vector3 a) where
    (==) (Vector3 (x1, y1, z1)) (Vector3 (x2, y2, z2)) = x1 == x2 && y1 == y2 && z1 == z2

getVector2Distance :: Floating a => Vector2 a -> Vector2 a -> a
getVector2Distance (Vector2 (x1, y1)) (Vector2 (x2, y2)) = sqrt ((x1 - x2) ^ (2 :: Integer) + (y1 - y2) ^ (2 :: Integer))

getVector3Distance :: Floating a => Vector3 a -> Vector3 a -> a
getVector3Distance (Vector3 (x1, y1, z1)) (Vector3 (x2, y2, z2)) = sqrt ((x1 - x2) ^ (2 :: Integer) + (y1 - y2) ^ (2 :: Integer) + (z1 - z2) ^ (2 :: Integer))

getVector2Length :: Floating a => Vector2 a -> a
getVector2Length (Vector2 (x, y)) = sqrt (x ^ (2 :: Integer) + y ^ (2 :: Integer))

getVector3Length :: Floating a => Vector3 a -> a
getVector3Length (Vector3 (x, y, z)) = sqrt (x ^ (2 :: Integer) + y ^ (2 :: Integer) + z ^ (2 :: Integer))

vector2itf :: Vector2 Int -> Vector2 Float
vector2itf (Vector2 (x, y))    = Vector2 (fromIntegral x, fromIntegral y)

vector3itf :: Vector3 Int -> Vector3 Float
vector3itf (Vector3 (x, y, z)) = Vector3 (fromIntegral x, fromIntegral y, fromIntegral z)

vector2fti :: Vector2 Float -> Vector2 Int
vector2fti (Vector2 (x, y))    = Vector2 (round x, round y)

vector3fti :: Vector3 Float -> Vector3 Int
vector3fti (Vector3 (x, y, z)) = Vector3 (round x, round y, round z)
