--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- Compressor
--

module Compressor         ( compress ) where

import Data.List          ( sort )
import System.Random      ( RandomGen )

import CompressorConf     ( CompressorConf(..) )
import FileParsing.Pixel  ( ColorRGB, Pixel(..) )
import RandomManager      ( makeNbRandomsUnique )
import Vector             ( Vector3(..)
                          , getLengthVector3
                          , vector3itn )
import Cluster            ( Move3D
                          , ClusterPos
                          , Cluster(..)
                          , Index
                          , applyMove
                          , generateMoves
                          , findClosestCluster )

type PrevClusterPos = ClusterPos

compress :: (RandomGen a) => a -> CompressorConf -> [Cluster]
compress _    (CompressorConf 0        _     _     ) = []
compress _    (CompressorConf _        _     []    ) = []
compress seed (CompressorConf nbColors limit pixels) = linkPixelsToClusters pixels $ adjustClusters limit pixels $ extractFirstClustersPos pixels $ makeNbRandomsUnique seed nbColors $ length pixels - 1


extractFirstClustersPos :: [Pixel] -> [Index] -> [ClusterPos]
extractFirstClustersPos pixels idxs = extractFirstClustersPos' 0 pixels $ sort idxs

extractFirstClustersPos' :: Index -> [Pixel] -> [Index] -> [ClusterPos]
extractFirstClustersPos' _ _                  []              = []
extractFirstClustersPos' _ []                 _               = []
extractFirstClustersPos' i (Pixel _ col : ps) idxs@(idx : xs)
    | idx == i  = vector3itn col : extractFirstClustersPos' (i + 1) ps xs
    | otherwise = extractFirstClustersPos' (i + 1) ps idxs


adjustClusters :: Float -> [Pixel] -> [ClusterPos] -> ([ClusterPos], [PrevClusterPos])
adjustClusters limit ps clusters = adjustClusters' limit ps (clusters, clusters) [Vector3 0 0 0]

adjustClusters' :: Float -> [Pixel] -> ([ClusterPos], [PrevClusterPos]) -> [Move3D] -> ([ClusterPos], [PrevClusterPos])
adjustClusters' _     _  res         []   = res
adjustClusters' limit ps (prevcs, _) move = let cs = applyMove prevcs move in adjustClusters' limit ps (cs, prevcs) $ filterMoves limit $ generateMoves cs $ generatePixelGroups ps cs


filterMoves :: Float -> [Move3D] -> [Move3D]
filterMoves limit moves = filterMoves' limit moves moves $ length moves

filterMoves' :: Float -> [Move3D] -> [Move3D] -> Int -> [Move3D]
filterMoves' _     _     []             0    = []
filterMoves' _     moves _              0    = moves
filterMoves' _     _     []             _    = []
filterMoves' limit moves mvs@(pos : ms) left
    | getLengthVector3 pos <= limit = filterMoves' limit moves ms  $ left - 1
    | otherwise                     = filterMoves' limit moves mvs $ left - 1


generatePixelGroups :: [Pixel] -> [ClusterPos] -> [(Int, ColorRGB)]
generatePixelGroups ps cs = generatePixelGroups' ps cs $ generateEmptyTotals $ length cs

generatePixelGroups' :: [Pixel] -> [ClusterPos] -> [(Int, ColorRGB)] -> [(Int, ColorRGB)]
generatePixelGroups' []                 _  res    = res
generatePixelGroups' (Pixel _ col : ps) cs totals = generatePixelGroups' ps cs $ insertColor totals col $ findClosestCluster cs col


generateEmptyTotals :: Int -> [(Int, ColorRGB)]
generateEmptyTotals 0   = []
generateEmptyTotals idx = (0, Vector3 0 0 0) : generateEmptyTotals (idx - 1)



-- [
--     (1, 2, 3), -- A
--     (1, 2, 3), -- B
--     (1, 2, 3), -- C
-- ]
-- [
--     (1, (255,       255,       255)), -- A
--     (6, (255 + 233, 255 + 244, 255 + 255)), -- B
--     (3, (255,       255,       255)), -- C
-- ]
insertColor :: [(Int, ColorRGB)] -> ColorRGB -> Index -> [(Int, ColorRGB)]
insertColor = insertColor' 0

insertColor' :: Index -> [(Int, ColorRGB)] -> ColorRGB -> Index -> [(Int, ColorRGB)]
insertColor' _ []                              _                    _   = []
insertColor' i (t@(nb, Vector3 tx ty tz) : ts) c@(Vector3 cx cy cz) idx
    | i == idx  = (nb + 1, Vector3 (tx + cx) (ty + cy) (tz + cz)) : ts
    | otherwise = t : insertColor' (i + 1) ts c idx


linkPixelsToClusters :: [Pixel] -> ([ClusterPos], [PrevClusterPos]) -> [Cluster]
linkPixelsToClusters ps (cs, prev) = deletePrevCluster $ foldr (\p@(Pixel _ col) acc -> insertPixel acc p $ findClosestCluster prev col) (zipWith (\c pr -> ((c, pr), [])) cs prev) ps


insertPixel :: [((ClusterPos, ClusterPos), [Pixel])] -> Pixel -> Int -> [((ClusterPos, ClusterPos), [Pixel])]
insertPixel = insertPixel' 0

insertPixel' :: Int -> [((ClusterPos, ClusterPos), [Pixel])] -> Pixel -> Int -> [((ClusterPos, ClusterPos), [Pixel])]
insertPixel' _ []                _ _    = []
insertPixel' i (r@(cs, ps) : rs) p idx
    | i == idx  = (cs, p : ps) : rs
    | otherwise = r : insertPixel' (i + 1) rs p idx


deletePrevCluster :: [((ClusterPos, ClusterPos), [Pixel])] -> [Cluster]
deletePrevCluster = foldr (\((cs, _), ps) acc -> Cluster cs ps : acc) []
