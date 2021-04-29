--
-- EPITECH PROJECT, 2021
-- ImageCompressor
-- File description:
-- Compressor
--

-- module Compressor ( compress ) where
module Compressor where

-- import Cluster ( Cluster )
import CreatePixel ( Pixel(..) )
import CompressorConf ( CompressorConf(..) )
import System.Random ( randomR, RandomGen, newStdGen )
import Data.List ( sort, length )
import System.IO.Unsafe

type Pos3D      = (Float, Float, Float)
type ClusterPos = Pos3D
type Cluster    = ((ClusterPos, ClusterPos), [Pixel])

compress :: (RandomGen a) => a -> CompressorConf -> [Cluster]
compress _    (CompressorConf 0        _       _     ) = []
compress _    (CompressorConf _        _       []    ) = []
compress seed (CompressorConf nbColors nbLimit pixels) = linkPixelsToClusters pixels $ generateClusters nbLimit pixels $ createFirstClustersPos pixels $ makeNbRandoms seed nbColors $ length pixels - 1

makeNbRandoms :: (RandomGen a) => a -> Int -> Int -> [Int]
makeNbRandoms _ 0 _ = []
makeNbRandoms seed nbLeft maxValue = let (val, newSeed) = randomR (0, maxValue) seed in val : makeNbRandoms newSeed (nbLeft - 1) maxValue
-- makeNbRandoms _ _ _ = [7, 8]

createFirstClustersPos :: [Pixel] -> [Int] -> [ClusterPos]
createFirstClustersPos pixels idxs = createFirstClustersPos' pixels 0 $ sort idxs
-- createFirstClustersPos _  _ = [(33, 18, 109), (35, 18, 109)]

createFirstClustersPos' :: [Pixel] -> Int -> [Int] -> [ClusterPos]
createFirstClustersPos' _  _ [] = []
createFirstClustersPos' [] _ _  = []
createFirstClustersPos' (Pixel _ (r, g, b) : ps) idx idxs@(x:xs) | x == idx  = (fromIntegral r, fromIntegral g, fromIntegral b) : createFirstClustersPos' ps (idx + 1) xs
                                                              | otherwise = createFirstClustersPos' ps (idx + 1) idxs

linkPixelsToClusters :: [Pixel] -> ([ClusterPos], [ClusterPos]) -> [Cluster]
linkPixelsToClusters ps (cs, prev) = foldr (\p@(Pixel _ col) acc -> insertPixel acc p $ findIdx prev col) (zipWith (\c pr -> ((c, pr), [])) cs prev) ps

insertPixel :: [Cluster] -> Pixel -> Int -> [Cluster]
insertPixel = insertPixel' 0

insertPixel' :: Int -> [Cluster] -> Pixel -> Int -> [Cluster]
insertPixel' i (r@(cs, ps):rs) p idx | i == idx  = (cs, p : ps) : rs
                                     | otherwise = r : insertPixel' (i + 1) rs p idx

generateClusters :: Float -> [Pixel] -> [ClusterPos] -> ([ClusterPos], [ClusterPos])
generateClusters limit ps clusters = gen limit ps (clusters, clusters) [(0, (0, 0, 0))]

type Color  = (Int, Int, Int)
type Move3D = (Float, Float, Float)

gen :: Float -> [Pixel] -> ([ClusterPos], [ClusterPos]) -> [(Int, Move3D)] -> ([ClusterPos], [ClusterPos])
gen _     _  tcs     []   = tcs
gen limit ps (cs, _) move = let ncs = appMove cs move in gen limit ps (ncs, cs) $ filterMoves limit $ genMove ncs $ genl ps ncs

appMove :: [ClusterPos] -> [(Int, Move3D)] -> [ClusterPos]
appMove = appMove' 0

appMove' :: Int -> [ClusterPos] -> [(Int, Move3D)] -> [ClusterPos]
appMove' _ []                 _  = []
appMove' _ cs                 [] = cs
appMove' i (c@(x, y, z) : cs) moves@((idx, (mx, my, mz)) : ms) | i == idx  = (x + mx, y + my, z + mz) : appMove' (i + 1) cs ms
                                                               | otherwise = c : appMove' (i + 1) cs moves

genMove :: [ClusterPos] -> [(Int, Color)] -> [(Int, Move3D)]
genMove = genMove' 0

genMove' :: Int -> [ClusterPos] -> [(Int, Color)] -> [(Int, Move3D)]
genMove' _ []               _                   = []
genMove' i ((x, y, z) : cs) ((nb, (cx, cy, cz)) : ts) = (i, (safeDiv (fromIntegral cx) (fromIntegral nb) - x, safeDiv (fromIntegral cy) (fromIntegral nb) - y, safeDiv (fromIntegral cz) (fromIntegral nb) - z)) : genMove' (i + 1) cs ts

safeDiv :: Float -> Float -> Float
safeDiv _ 0 = 0
safeDiv a b = a / b


logIO :: Show a => a -> IO a
logIO var = do print var
               return var

logVar :: Show a => a -> a
logVar var = unsafePerformIO $ logIO var

filterMoves :: Float -> [(Int, Move3D)] -> [(Int, Move3D)]
filterMoves limit moves = filterMoves' limit moves moves $ length moves

filterMoves' :: Float -> [(Int, Move3D)] -> [(Int, Move3D)] -> Int -> [(Int, Move3D)]
filterMoves' limit moves [] 0 = []
filterMoves' limit moves _  0 = moves
filterMoves' limit moves mvs@(m@(_, pos) : ms) left | vect3dLength pos <= limit = filterMoves' limit moves ms  (left - 1)
                                                    | otherwise                 = filterMoves' limit moves mvs (left - 1)

vect3dLength :: (Float, Float, Float) -> Float
vect3dLength (x, y, z) = sqrt (x ^ 2 + y ^ 2 + z ^ 2)

genl :: [Pixel] -> [ClusterPos] -> [(Int, Color)]
genl ps cs = genl' ps cs $ generateEmptyTotals $ length cs

genl' :: [Pixel] -> [ClusterPos] -> [(Int, Color)] -> [(Int, Color)]
genl' []                 _  r = r
genl' (Pixel _ col : ps) cs r = genl' ps cs $ insertCol r col $ findIdx cs col

generateEmptyTotals :: Int -> [(Int, Color)]
generateEmptyTotals 0   = []
generateEmptyTotals idx = (0, (0, 0, 0)) : generateEmptyTotals (idx - 1)

findIdx :: [ClusterPos] -> Color -> Int
findIdx cs@(c:_) col = findIdx' cs col c 0 0

findIdx' :: [ClusterPos] -> Color -> ClusterPos -> Int -> Int -> Int
findIdx' []     _   _    _ idx = idx
findIdx' (c:cs) col best i idx | get3dDistance c (tupleToFloat col) < get3dDistance best (tupleToFloat col) = findIdx' cs col c    (i + 1) i
                               | otherwise                                                                  = findIdx' cs col best (i + 1) idx

tupleToFloat :: (Int, Int, Int) -> (Float, Float, Float)
tupleToFloat (x, y, z) = (fromIntegral x, fromIntegral y, fromIntegral z)

insertCol :: [(Int, Color)] -> Color -> Int -> [(Int, Color)]
insertCol = insertCol' 0

insertCol' :: Int -> [(Int, Color)] -> Color -> Int -> [(Int, Color)]
insertCol' i (r@(nb, (x, y, z)):rs) c@(cx, cy, cz) idx | i == idx  = (nb + 1, (x + cx, y + cy, z + cz)) : rs
                                                       | otherwise = r : insertCol' (i + 1) rs c idx











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

get3dDistance :: (Float, Float, Float) -> (Float, Float, Float) -> Float
get3dDistance (r2, g2, b2) (r1, g1, b1) = sqrt ((r1 - r2) ^ 2 + (g1 - g2) ^ 2 + (b1 - b2) ^ 2)

-- getMeanOfPixels :: [Pixel] -> (Float, Float, Float)
-- getMeanOfPixels pixels = getMeanOfPixels' pixels (0, 0, 0) $ length pixels

-- getMeanOfPixels' :: [Pixel] -> (Float, Float, Float) -> Int -> (Float, Float, Float)
-- getMeanOfPixels' []                          (r, g, b)    n = (r / fromIntegral n, g / fromIntegral n, b / fromIntegral n)
-- getMeanOfPixels' ((Pixel _ (r1, g1, b1)):ps) (r2, g2, b2) n = getMeanOfPixels' ps (fromIntegral r1 + r2, fromIntegral g1 + g2, fromIntegral b1 + b2) n
