{-# LANGUAGE RecordWildCards #-}
module Analysis where
import Data.Maybe(catMaybes)
import Data.List(sort,group,foldl',(\\))

import Core
import Containers
import Operations

{-
data State = State { clusterList :: ClusterList
                   , groupRib :: GroupRib
                   , prefixRib :: PrefixRib
                   } deriving Show

clusters :: State -> [ Cluster ]
clusters = elems . clusterList

groups :: State -> [ CompositeGroup ]
groups = elems . groupRib

-}

analysis :: State -> String
analysis s = unlines results
    where
    results = [ analysis1 s, analysis2 s ]

    analysis1 = partitionOrderAnalysis
    analysis2 _ = "analysis2"

partitionOrderAnalysis :: State -> String
partitionOrderAnalysis State{..} = "partitionOrderAnalysis: " ++ show ( histogram partionSizes )
    where
    partionSizes = map ( fromIntegral . length . clBasicGroups) (elems clusterList)

histogram :: [Int] -> [(Int, Int)]
histogram = map countAndTell . group . sort
    where
    countAndTell ax = (head ax, fromIntegral $ length ax)
