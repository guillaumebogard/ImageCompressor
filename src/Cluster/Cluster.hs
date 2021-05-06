--
-- EPITECH PROJECT, 2021
-- B-FUN-400-BDX-4-1-compressor-guillaume.bogard-coquard
-- File description:
-- Cluster
--

module Cluster.Cluster where

import Vector.Vector
import FileParsing.Pixel

--data Cluster = Cluster (Float, Float, Float) deriving Show

type Move3D     = (Float, Float, Float)

type ClusterPos = Vector3 Float
type Cluster    = ((ClusterPos, ClusterPos), [Pixel]) -- make data, deriving show for cluster printing

appMove :: [ClusterPos] -> [Move3D] -> [ClusterPos]
appMove []               _                   = []
appMove cs               []                  = cs
appMove (Vector3 (x, y, z) : cs) ((mx, my, mz) : ms) = Vector3  (x + mx, y + my, z + mz) : appMove cs ms

genMove :: [ClusterPos] -> [(Int, ColorRGB)] -> [Move3D]
genMove = zipWith genMove'

genMove' :: ClusterPos -> (Int, ColorRGB) -> Move3D
genMove' _         (0,  _)            = (0, 0, 0)
genMove' (Vector3 (x, y, z)) (nb, Vector3 (cx, cy, cz)) = ( cx `safeDivToFloat` nb - x
                                                            , cy `safeDivToFloat` nb - y
                                                            , cz `safeDivToFloat` nb - z)

safeDivToFloat :: Int -> Int -> Float
safeDivToFloat _ 0 = 0
safeDivToFloat a b = fromIntegral a / fromIntegral b

findIdx :: [ClusterPos] -> ColorRGB -> Int
findIdx []     _   = -1
findIdx (c:cs) col = findIdx' col 1 cs c 0 $ getVector3Distance c $ vector3itf col

findIdx' :: ColorRGB -> Int -> [ClusterPos] -> ClusterPos -> Int -> Float -> Int
findIdx' _   _ []     _    idx     _                       = idx
findIdx' col i (c:cs) best bestIdx bestDist
        | getVector3Distance c (vector3itf col) < bestDist = findIdx' col (i + 1) cs c i $ getVector3Distance c $ vector3itf col
        | otherwise                                        = findIdx' col (i + 1) cs best bestIdx bestDist
