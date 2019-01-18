{-# LANGUAGE RecordWildCards #-}
module Operations where
import Data.Maybe(isJust,fromJust)
import Data.List(sort,nub,foldl')

import Core
import Containers
import BasicOperations

data State = State { clusterList :: ClusterList
                   , groupRib :: GroupRib
                   , prefixRib :: PrefixRib
                   } deriving Show

clusters :: State -> [ Cluster ]
clusters = elems . clusterList

newState :: State
newState = State emptyClusterList emptyGroupRib emptyPrefixRib

ribUpdate :: PrefixList -> State -> State
ribUpdate pl0 s = if present then error "trying to insert an existing group" else State newClusterList newGroupRib newPrefixRib
    where

    -- sanity check only .....
    present = isJust (Containers.lookup (prefixListHash pl0) (groupRib s) )

    (clusterMap,unmatchedPrefixList) = getClusterMap pl0 (prefixRib s)

    (tmpCluster,tmpCompositeGroup) = updateClusters clusterMap
    markedClusters = map fst clusterMap
    newBasicGroup = mkBasicGroup unmatchedPrefixList
    newCompositeGroup = mergeCompositeGroups [tmpCompositeGroup, mkCompositeGroup [newBasicGroup]]

    -- build the new cluster
    newCluster = mkCluster (newCompositeGroup : clCompositeGroups tmpCluster) (newBasicGroup : clBasicGroups tmpCluster)

    -- now update the RIB state
    newClusterList = updateClusterList newCluster markedClusters (clusterList s)
    newGroupRib    = updateGroupRib (clCompositeGroups newCluster) (groupRib s)
    newPrefixRib   = updatePrefixRib newCluster (prefixRib s)

    getClusterMap :: PrefixList -> PrefixRib -> ( [(Cluster,PrefixList)] , PrefixList )
    getClusterMap pl pr = (clusters, sort unmatched)
        where
        clusters = getClusters $ rollUp matched
        getClusters :: [(Hash,PrefixList)] -> [(Cluster,PrefixList)]
        getClusters = map (\(hash,pfxs) -> (fromJust $ Containers.lookup hash (clusterList s),pfxs))
        rollUp l = map (\p -> (p, map snd (filter ((p ==) . fst) l))) (nub $ map fst l)
        (matched,unmatched) = foldl' (\(m,u) pfx -> maybe (m,pfx:u)
                                                         (\g -> ((g,pfx):m,u))
                                                         (Containers.lookup (prefixHash pfx) pr )
                                    )
                                    ([],[])
                                    pl

    updateClusters :: [(Cluster,PrefixList)] -> (Cluster,CompositeGroup)
    updateClusters ax = (mergeClusters cls, mergeCompositeGroups cgs) where
        (cls,cgs) = mergeUpdates $ map updateCluster ax

        mergeUpdates :: [(Cluster,CompositeGroup)] -> ([Cluster],[CompositeGroup])
        mergeUpdates = foldl' (\(acca,accb) (a,b) -> (a:acca, b:accb)) ([],[])

        updateCluster :: (Cluster,PrefixList) -> (Cluster,CompositeGroup)
        updateCluster (Cluster _ cgs bgs,pl) = (mkCluster newCompositeGroups newBasicGroups , targetCompositeGroup)
            where
            (newBasicGroups, targetCompositeGroup, editList) = updateBasicGroups bgs pl
            newCompositeGroups = updateCompositeGroups editList cgs

displayState :: State -> String
displayState State{..} = "clusters: " ++ show (length clusterList)
                       ++ " groups: " ++ show (length groupRib)
                       ++ " prefixes: " ++ show (length prefixRib)
