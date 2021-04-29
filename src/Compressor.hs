--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- Compressor
--

-- module Compressor ( compress ) where
module Compressor where

-- import Cluster ( Cluster )
import CreatePixel ( Pixel(..), ColorRGB )
import CompressorConf ( CompressorConf(..) )
import System.Random ( randomR, RandomGen )
import Data.List ( sort )
import System.IO.Unsafe

type Pos3D      = (Float, Float, Float)
type ClusterPos = Pos3D
type Cluster    = ((ClusterPos, ClusterPos), [Pixel])
type Move3D     = (Float, Float, Float)

compress :: (RandomGen a) => a -> CompressorConf -> [Cluster]
compress _    (CompressorConf 0        _       _     ) = []
compress _    (CompressorConf _        _       []    ) = []
compress seed (CompressorConf nbColors nbLimit pixels) = linkPixelsToClusters pixels $ generateClusters nbLimit pixels $ createFirstClustersPos pixels $ makeNbRandoms seed nbColors $ length pixels - 1

makeNbRandoms :: (RandomGen a) => a -> Int -> Int -> [Int]
makeNbRandoms _    0      _        = []
makeNbRandoms seed nbLeft maxValue = let (val, newSeed) = randomR (0, maxValue) seed in val : makeNbRandoms newSeed (nbLeft - 1) maxValue

createFirstClustersPos :: [Pixel] -> [Int] -> [ClusterPos]
createFirstClustersPos pixels idxs = createFirstClustersPos' 0 pixels $ sort idxs

createFirstClustersPos' :: Int -> [Pixel] -> [Int] -> [ClusterPos]
createFirstClustersPos' _   _                  []           = []
createFirstClustersPos' _   []                 _            = []
createFirstClustersPos' idx (Pixel _ col : ps) idxs@(x:xs)  | x == idx  = tupleToFloat col : createFirstClustersPos' (idx + 1) ps xs
                                                            | otherwise = createFirstClustersPos' (idx + 1) ps idxs

generateClusters :: Float -> [Pixel] -> [ClusterPos] -> ([ClusterPos], [ClusterPos])
generateClusters limit ps clusters = generateClusters' limit ps (clusters, clusters) [(0, (0, 0, 0))]

generateClusters' :: Float -> [Pixel] -> ([ClusterPos], [ClusterPos]) -> [(Int, Move3D)] -> ([ClusterPos], [ClusterPos])
generateClusters' _     _  res         []   = res
generateClusters' limit ps (prevcs, _) move = let cs = appMove prevcs move in generateClusters' limit ps (cs, prevcs) $ filterMoves limit $ genMove cs $ genOneTime ps cs

appMove :: [ClusterPos] -> [(Int, Move3D)] -> [ClusterPos]
appMove = appMove' 0

appMove' :: Int -> [ClusterPos] -> [(Int, Move3D)] -> [ClusterPos]
appMove' _ []                 _                                = []
appMove' _ cs                 []                               = cs
appMove' i (c@(x, y, z) : cs) moves@((idx, (mx, my, mz)) : ms) | i == idx  = (x + mx, y + my, z + mz) : appMove' (i + 1) cs ms
                                                               | otherwise = c : appMove' (i + 1) cs moves

genMove :: [ClusterPos] -> [(Int, ColorRGB)] -> [(Int, Move3D)]
genMove = genMove' 0

genMove' :: Int -> [ClusterPos] -> [(Int, ColorRGB)] -> [(Int, Move3D)]
genMove' _ []               _                         = []
genMove' _ _                []                        = []
genMove' i ((x, y, z) : cs) ((nb, (cx, cy, cz)) : ts) = (i, (fromIntegral cx `floatSafeDiv` fromIntegral nb - x
                                                            , fromIntegral cy `floatSafeDiv` fromIntegral nb - y
                                                            , fromIntegral cz `floatSafeDiv` fromIntegral nb - z)) : genMove' (i + 1) cs ts

floatSafeDiv :: Float -> Float -> Float
floatSafeDiv _ 0 = 0
floatSafeDiv a b = a / b


logIO :: Show a => a -> IO a
logIO var = do print var
               return var

logVar :: Show a => a -> a
logVar var = unsafePerformIO $ logIO var

filterMoves :: Float -> [(Int, Move3D)] -> [(Int, Move3D)]
filterMoves limit moves = filterMoves' limit moves moves $ length moves

filterMoves' :: Float -> [(Int, Move3D)] -> [(Int, Move3D)] -> Int -> [(Int, Move3D)]
filterMoves' _     _     []                    0    = []
filterMoves' _     moves _                     0    = moves
filterMoves' _     _     []                    _    = []
filterMoves' limit moves mvs@((_, pos) : ms) left | vect3dLength pos <= limit = filterMoves' limit moves ms  $ left - 1
                                                    | otherwise                 = filterMoves' limit moves mvs $ left - 1

vect3dLength :: (Float, Float, Float) -> Float
vect3dLength (x, y, z) = sqrt (x ^ (2 :: Integer) + y ^ (2 :: Integer) + z ^ (2 :: Integer))

genOneTime :: [Pixel] -> [ClusterPos] -> [(Int, ColorRGB)]
genOneTime ps cs = genOneTime' ps cs $ generateEmptyTotals $ length cs

genOneTime' :: [Pixel] -> [ClusterPos] -> [(Int, ColorRGB)] -> [(Int, ColorRGB)]
genOneTime' []                 _  res    = res
genOneTime' (Pixel _ col : ps) cs totals = genOneTime' ps cs $ insertCol totals col $ findIdx cs col

generateEmptyTotals :: Int -> [(Int, ColorRGB)]
generateEmptyTotals 0   = []
generateEmptyTotals idx = (0, (0, 0, 0)) : generateEmptyTotals (idx - 1)

findIdx :: [ClusterPos] -> ColorRGB -> Int
findIdx []     _   = -1
findIdx (c:cs) col = findIdx' col 1 cs c 0 $ get3dDistance c $ tupleToFloat col

findIdx' :: ColorRGB -> Int -> [ClusterPos] -> ClusterPos -> Int -> Float -> Int
findIdx' _   _ []     _    idx     _        = idx
findIdx' col i (c:cs) best bestIdx bestDist | get3dDistance c (tupleToFloat col) < bestDist = findIdx' col (i + 1) cs c    i       $ get3dDistance c $ tupleToFloat col
                                            | otherwise                                     = findIdx' col (i + 1) cs best bestIdx bestDist

tupleToFloat :: (Int, Int, Int) -> (Float, Float, Float)
tupleToFloat (x, y, z) = (fromIntegral x, fromIntegral y, fromIntegral z)

insertCol :: [(Int, ColorRGB)] -> ColorRGB -> Int -> [(Int, ColorRGB)]
insertCol = insertCol' 0

insertCol' :: Int -> [(Int, ColorRGB)] -> ColorRGB -> Int -> [(Int, ColorRGB)]
insertCol' _ []                          _              _   = []
insertCol' i (t@(nb, (tx, ty, tz)) : ts) c@(cx, cy, cz) idx | i == idx  = (nb + 1, (tx + cx, ty + cy, tz + cz)) : ts
                                                            | otherwise = t : insertCol' (i + 1) ts c idx

linkPixelsToClusters :: [Pixel] -> ([ClusterPos], [ClusterPos]) -> [Cluster]
linkPixelsToClusters ps (cs, prev) = foldr (\p@(Pixel _ col) acc -> insertPixel acc p $ findIdx prev col) (zipWith (\c pr -> ((c, pr), [])) cs prev) ps

insertPixel :: [Cluster] -> Pixel -> Int -> [Cluster]
insertPixel = insertPixel' 0

insertPixel' :: Int -> [Cluster] -> Pixel -> Int -> [Cluster]
insertPixel' _ []                _ _    = []
insertPixel' i (r@(cs, ps) : rs) p idx  | i == idx  = (cs, p : ps) : rs
                                        | otherwise = r : insertPixel' (i + 1) rs p idx

get3dDistance :: (Float, Float, Float) -> (Float, Float, Float) -> Float
get3dDistance (r2, g2, b2) (r1, g1, b1) = sqrt ((r1 - r2) ^ (2 :: Integer) + (g1 - g2) ^ (2 :: Integer) + (b1 - b2) ^ (2 :: Integer))








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
