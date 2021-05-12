--
-- EPITECH PROJECT, 2021
-- B-FUN-400-BDX-4-1-compressor-guillaume.bogard-coquard
-- File description:
-- Cluster
--

module Cluster            ( Move3D
                          , ClusterPos
                          , Cluster(..)
                          , Index
                          , applyMove
                          , generateMoves
                          , findClosestCluster
                          ) where

import Vector             ( Vector3(..)
                          , vector3fti
                          , getDistanceVector3
                          , vector3itn )
import FileParsing.Pixel  ( ColorRGB
                          , Pixel )

type Move3D     = Vector3 Float
type Index      = Int
type ClusterPos = Vector3 Float

data Cluster    = Cluster ClusterPos [Pixel]
instance Show Cluster where
    show (Cluster pos ps) = "--\n" ++ show (vector3fti pos) ++ "\n-" ++ concatMap (\p -> '\n' : show p) ps
instance Eq Cluster where
    (==) (Cluster pos1 ps1) (Cluster pos2 ps2) = pos1 == pos2 && ps1 == ps2

applyMove :: [ClusterPos] -> [Move3D] -> [ClusterPos]
applyMove []                   _                       = []
applyMove cs                   []                      = cs
applyMove (Vector3 x y z : cs) (Vector3 mx my mz : ms) = Vector3 (x + mx) (y + my) (z + mz) : applyMove cs ms

generateMoves :: [ClusterPos] -> [(Int, ColorRGB)] -> [Move3D]
generateMoves = zipWith generateMoves'

generateMoves' :: ClusterPos -> (Int, ColorRGB) -> Move3D
generateMoves' _               (0,  _)                = Vector3 0 0 0
generateMoves' (Vector3 x y z) (nb, Vector3 cx cy cz) = Vector3 (cx `safeDivToFloat` nb - x) (cy `safeDivToFloat` nb - y) (cz `safeDivToFloat` nb - z)

safeDivToFloat :: Int -> Int -> Float
safeDivToFloat _ 0 = 0
safeDivToFloat a b = fromIntegral a / fromIntegral b

findClosestCluster :: [ClusterPos] -> ColorRGB -> Index
findClosestCluster []     _   = -1
findClosestCluster (c:cs) col = findClosestCluster' col 1 cs c 0 $ getDistanceVector3 c $ vector3itn col

findClosestCluster' :: ColorRGB -> Index -> [ClusterPos] -> ClusterPos -> Index -> Float -> Index
findClosestCluster' _   _ []     _    idx     _        = idx
findClosestCluster' col i (c:cs) best bestIdx bestDist
    | getDistanceVector3 c (vector3itn col) < bestDist = findClosestCluster' col (i + 1) cs c i $ getDistanceVector3 c $ vector3itn col
    | otherwise                                        = findClosestCluster' col (i + 1) cs best bestIdx bestDist
