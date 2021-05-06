--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- Compressor
--

-- module Compressor ( compress ) where
module Compressor where

-- import Cluster ( Cluster )
import FileParsing.CreatePixel
import FileParsing.Pixel
import CompressorConf ( CompressorConf(..) )
import System.Random ( randomR, RandomGen )
import Data.List ( sort, nub )
import RandomManager ( makeNbRandomsUnique )
import Vector.Vector
import Cluster.Cluster

-- use newtypes or datas instead of types

compress :: (RandomGen a) => a -> CompressorConf -> [Cluster]
compress _    (CompressorConf 0        _       _     ) = []
compress _    (CompressorConf _        _       []    ) = []
compress seed (CompressorConf nbColors limit pixels) = linkPixelsToClusters pixels $ generateClusters limit pixels $ createFirstClustersPos pixels $ makeNbRandomsUnique seed nbColors $ length pixels - 1

createFirstClustersPos :: [Pixel] -> [Int] -> [ClusterPos]
createFirstClustersPos pixels idxs = createFirstClustersPos' 0 pixels $ sort idxs

createFirstClustersPos' :: Int -> [Pixel] -> [Int] -> [ClusterPos]
createFirstClustersPos' _   _                  []           = []
createFirstClustersPos' _   []                 _            = []
createFirstClustersPos' idx (Pixel _ col : ps) idxs@(x:xs)  | x == idx  = vector3itf col : createFirstClustersPos' (idx + 1) ps xs
                                                            | otherwise = createFirstClustersPos' (idx + 1) ps idxs

generateClusters :: Float -> [Pixel] -> [ClusterPos] -> ([ClusterPos], [ClusterPos])
generateClusters limit ps clusters = generateClusters' limit ps (clusters, clusters) [Vector3 (0, 0, 0)]

generateClusters' :: Float -> [Pixel] -> ([ClusterPos], [ClusterPos]) -> [Move3D] -> ([ClusterPos], [ClusterPos])
generateClusters' _     _  res         []   = res
generateClusters' limit ps (prevcs, _) move = let cs = appMove prevcs move in generateClusters' limit ps (cs, prevcs) $ filterMoves limit $ genMove cs $ genOneTime ps cs

filterMoves :: Float -> [Move3D] -> [Move3D]
filterMoves limit moves = filterMoves' limit moves moves $ length moves

filterMoves' :: Float -> [Move3D] -> [Move3D] -> Int -> [Move3D]
filterMoves' _     _     []             0       = []
filterMoves' _     moves _              0       = moves
filterMoves' _     _     []             _       = []
filterMoves' limit moves mvs@(pos : ms) left
                    | getVector3Length pos <= limit = filterMoves' limit moves ms  $ left - 1
                    | otherwise                 = filterMoves' limit moves mvs $ left - 1

genOneTime :: [Pixel] -> [ClusterPos] -> [(Int, ColorRGB)]
genOneTime ps cs = genOneTime' ps cs $ generateEmptyTotals $ length cs

genOneTime' :: [Pixel] -> [ClusterPos] -> [(Int, ColorRGB)] -> [(Int, ColorRGB)]
genOneTime' []                 _  res    = res
genOneTime' (Pixel _ col : ps) cs totals = genOneTime' ps cs $ insertCol totals col $ findIdx cs col

generateEmptyTotals :: Int -> [(Int, ColorRGB)]
generateEmptyTotals 0   = []
generateEmptyTotals idx = (0, Vector3 (0, 0, 0)) : generateEmptyTotals (idx - 1)

insertCol :: [(Int, ColorRGB)] -> ColorRGB -> Int -> [(Int, ColorRGB)]
insertCol = insertCol' 0

insertCol' :: Int -> [(Int, ColorRGB)] -> ColorRGB -> Int -> [(Int, ColorRGB)]
insertCol' _ []                          _              _   = []
insertCol' i (t@(nb, Vector3 (tx, ty, tz)) : ts) c@(Vector3 (cx, cy, cz)) idx
                                                | i == idx  = (nb + 1, Vector3 (tx + cx, ty + cy, tz + cz)) : ts
                                                | otherwise = t : insertCol' (i + 1) ts c idx

linkPixelsToClusters :: [Pixel] -> ([ClusterPos], [ClusterPos]) -> [Cluster]
linkPixelsToClusters ps (cs, prev) = foldr (\((cs, _), ps) acc -> Cluster (cs, ps) : acc) [] $ foldr (\p@(Pixel _ col) acc -> insertPixel acc p $ findIdx prev col) (zipWith (\c pr -> ((c, pr), [])) cs prev) ps

insertPixel :: [((ClusterPos, ClusterPos), [Pixel])] -> Pixel -> Int -> [((ClusterPos, ClusterPos), [Pixel])]
insertPixel = insertPixel' 0

insertPixel' :: Int -> [((ClusterPos, ClusterPos), [Pixel])] -> Pixel -> Int -> [((ClusterPos, ClusterPos), [Pixel])]
insertPixel' _ []                _ _    = []
insertPixel' i (r@(cs, ps) : rs) p idx  | i == idx  = (cs, p : ps) : rs
                                        | otherwise = r : insertPixel' (i + 1) rs p idx









-- computeCompression :: [Pixel] -> Float -> [(ClusterPos, [Pixel])]
-- computeCompression pixels limit = computeClusters pixels createFirstClustersPos limit

-- computeClusters :: [Pixel] -> [ClusterPos] -> [(ClusterPos), [Pixel]]
-- computeClusters pixels clusters limit
--    |  =
--    | otherwise =

-- associatePixelsToClusters :: [Pixel] -> [ClusterPos] -> [(ClusterPos, [Pixel])]
-- associatePixelsToClusters pixels clusters = associatePixelsToClusters' pixels clusters clusters

-- associatePixelsToClusters' :: [Pixel] -> [ClusterPos] -> [ClusterPos] -> [(ClusterPos, [Pixel])]
-- associatePixelsToClusters' _      _        []     = []
-- associatePixelsToClusters' pixels clusters (c:cs) = (c, getPixelsAssociatedWithCluster c pixels clusters) : associatePixelsToClusters' pixels clusters cs

-- getPixelsAssociatedWithCluster :: ClusterPos -> [Pixel] -> [ClusterPos] -> [Pixel]
-- getPixelsAssociatedWithCluster _       []     _        = []
-- getPixelsAssociatedWithCluster cluster (p:ps) clusters
--     | getClosestClusterFromPixel clusters p == cluster = p : getPixelsAssociatedWithCluster cluster ps clusters
--     | otherwise = getPixelsAssociatedWithCluster cluster ps clusters

-- getClosestClusterFromPixel :: [ClusterPos] -> Pixel -> ClusterPos
-- getClosestClusterFromPixel clusters@(c:_) pixel = getClosestClusterFromPixel' clusters pixel c

-- getClosestClusterFromPixel' :: [ClusterPos] -> Pixel -> ClusterPos -> ClusterPos
-- getClosestClusterFromPixel' []     _     current_cluster = current_cluster
-- getClosestClusterFromPixel' (c:cs) pixel current_cluster
--     | getClusterDistanceFromPixel current_cluster pixel > getClusterDistanceFromPixel c pixel = getClosestClusterFromPixel' cs pixel c
--     | otherwise = getClosestClusterFromPixel' cs pixel current_cluster

-- getClusterDistanceFromPixel :: ClusterPos -> Pixel -> Float
-- getClusterDistanceFromPixel cluster (Pixel _ (r, g, b)) = get3dDistance cluster (fromIntegral r, fromIntegral g, fromIntegral b)

-- getMeanOfPixels :: [Pixel] -> (Float, Float, Float)
-- getMeanOfPixels pixels = getMeanOfPixels' pixels (0, 0, 0) $ length pixels

-- getMeanOfPixels' :: [Pixel] -> (Float, Float, Float) -> Int -> (Float, Float, Float)
-- getMeanOfPixels' []                          (r, g, b)    n = (r / fromIntegral n, g / fromIntegral n, b / fromIntegral n)
-- getMeanOfPixels' ((Pixel _ (r1, g1, b1)):ps) (r2, g2, b2) n = getMeanOfPixels' ps (fromIntegral r1 + r2, fromIntegral g1 + g2, fromIntegral b1 + b2) n
