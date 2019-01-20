{-# LANGUAGE RecordWildCards #-}
module Analysis where
import Data.List(sort,group)
import Text.Printf

import Core
import Containers
import Operations

analysis :: State -> String
analysis s = unlines results
    where
    results = [ analysis1 s, analysis2 s, analysis3 s ]

    analysis1 = partitionOrderAnalysis
    analysis2 = clusterSizeAnalysis
    analysis3 = clusterGroupSizeAnalysis

partitionOrderAnalysis :: State -> String
partitionOrderAnalysis State{..} = "Partition Order Analysis: " ++ showHistogram partionSizes -- show ( histogram partionSizes )
    where
    partionSizes = map ( fromIntegral . length . clBasicGroups) (elems clusterList)

clusterSizeAnalysis :: State -> String
clusterSizeAnalysis State{..} = "Cluster Size Analysis: " ++ showHistogram clusterSizes -- show ( histogram clusterSizes )
    where
    clusterSizes = map ( fromIntegral . length . concatMap basicPrefixes . clBasicGroups) (elems clusterList)

clusterGroupSizeAnalysis :: State -> String
clusterGroupSizeAnalysis State{..} = "Cluster Group Size Analysis: " ++ showHistogram clusterSizes -- show ( histogram clusterSizes )
    where
    clusterSizes = map ( fromIntegral . length . clCompositeGroups) (elems clusterList)

histogram :: [Int] -> [(Int, Int)]
histogram = map countAndTell . group . sort
    where
    countAndTell ax = (head ax, fromIntegral $ length ax)

showHistogram :: [Int] -> String
showHistogram tx = show ( histogram tx ) ++ "\n" ++ showPercentoGram ( percentoGram $ histogram tx ) ++ "\n\n"

showPercentiles :: [Float] -> [(a, Int)] -> [(a, Float)]
-- 
-- show the first 'a' for which the cumulative count exceeds a specific percentage of all 'a's
--
-- start by counting the totals and then the corresponding (Int) values which must be met or exceeded to trigger a note
showPercentiles percentiles vals = reverse $ go 0 percentiles vals
    where
    count = length vals
    --breaks = map (\percentile -> ceiling ( percentile / 100 * fromIntegral count)) percentiles :: [Int]
    percentileToCount percentile = ceiling ( percentile / 100 * fromIntegral count)) percentiles :: [Int]
    go _ _ [] = []
    go _ [] _ = []
    go cnt (brk:brks) (a:ax) | (percentileToCount brk) < cnt = (a,brk) : go (cnt+1) brks ax
                             | otherwise = go res (cnt+1) (brk:brks) ax

showPercentoGram :: (Show a) => [(a, Float)] -> String
showPercentoGram = concatMap showAPercentoGram
    where showAPercentoGram (a,f) = "(" ++ show a ++ " , " ++ printf "%2.2f" f ++ ")"

percentoGram :: [(a, Int)] -> [(a, Float)]
-- display a percentage take on a histogram
-- snd parameter is a count, want to show this as either % or %.(1-).cumulative
percentoGram tx = asProportionSnd total (cumulativeSnd tx)
    where
    total = fromIntegral $ sum (map snd tx) :: Float
    float n = fromIntegral n :: Float
    cumulativeSnd = go 0
        where
        go _ [] = []
        go n ((a,b):abx) = (a,b+n) : go (b+n) abx
    asProportionSnd n = map (\(a,b) -> (a, 100 * float b/n))
    --asNegProportionSnd n = map (\(a,b) -> (a,n-b/n)
