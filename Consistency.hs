{-# LANGUAGE RecordWildCards #-}
module Consistency where
import Data.Maybe(catMaybes)
import Data.List(sort,nub,foldl')

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

consistency :: State -> Maybe String
consistency s = if null (catMaybes results) then Nothing else Just $ unlines (catMaybes results)
    where
    results = [ check1 s, check2 s ]

check1 :: State -> Maybe String
check1 _ = Nothing 

check2 :: State -> Maybe String
check2 _ = Nothing 
