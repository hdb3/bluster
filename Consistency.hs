{-# LANGUAGE RecordWildCards #-}
module Consistency where
import Data.Maybe(catMaybes)
import Data.List(sort,nub,foldl',(\\))

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

    check1 = checkPrefixes

    check2 = checkGroups

checkPrefixes :: State -> Maybe String
checkPrefixes st = if cPrefixes == ribPrefixes then Nothing else Just $
    "checkPrefixes FAIL: counts - " ++ show ( length cPrefixes) ++ " / " ++ show ( length ribPrefixes)
    ++ "\n " ++ show (cPrefixes \\ ribPrefixes)
    ++ "\n " ++ show (ribPrefixes \\ cPrefixes)
    where
    cPrefixes = sort $ concatMap clusterPrefixes (clusterList st)
    ribPrefixes = sort $ map Prefix $ keys $ prefixRib st

checkGroups :: State -> Maybe String
checkGroups st = if cGroups == ribGroups then Nothing else Just $
    "checkGroups FAIL: counts - " ++ show ( length cGroups) ++ " / " ++ show ( length ribGroups)
    ++ "\n " ++ show (cGroups \\ ribGroups)
    ++ "\n " ++ show (ribGroups \\ cGroups)
    where
    cGroups = sort $ concatMap clCompositeGroups $ elems (clusterList st)
    ribGroups = sort $ elems $ groupRib st

