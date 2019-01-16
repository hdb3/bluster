{-# LANGUAGE RecordWildCards #-}
module Operations where
import Data.Maybe(isJust)

import Core
import Containers

data State = State { clusterList :: ClusterList
                   , groupRib :: GroupRib
                   , prefixRib :: PrefixRib
                   } deriving Show

newState = State newClusterList newGroupRib newPrefixRib

ribUpdate :: PrefixList -> State -> State
--insertGroup _ a = a
--ribUpdate pl s = if present then s else State{..} where
ribUpdate pl s = if present then (error "trying to insert an existing group") else State{..} where
    hash = groupHash pl
    present = isJust (Map.lookup groups hash)
    (clusterMap,unmatchedPrefixList) = getClusterMap pl (prefixRib s)
    -- clusterMap = getClusterMap pl prefixes -- using RecordWildCards

    getClusterMap :: PrefixList -> PrefixRib -> ([(GroupHash,PrefixList),PrefixList)
    -- first value returns prefixes mapped already, second value is unmapped prefixes
    getClusterMap pl pr = (rollUp matched, sort unmatched) where
        --r = map (\pfx -> (pfx,Map.lookup pfx pr)) pl
        --(matched,unmatched) = foldl (\(m,u) t -> if isNothing (snd t) then (m,t:u) else (t:m,u)) ([],[]) r
        (matched,unmatched) = foldl (\(m,u) pfx -> maybe (m,pfx:u) (\g -> ((g,pfx):m,u)) Map.lookup pfx pr ) pl
        rollUp l = map (\p -> (p, map snd (filter ((p ==) . fst) l))) (nub $ map fst l)
    -- we must apply the new prefix lists for each cluster, accumulate the result, add in the new basic block to, and forn a new cluster
    -- the per cluster operation is updateCluster
    -- updateCluster returns the, possibly updated, list of groups in the cluster - this is used not only to form a new cluster
    -- but also to update entries in the group rib (new basic groups, replace some old basic groups with composite)
    -- finally, we require the list of basic subgroups which will populate the new group which represents the target
    -- the rrturn value is two lists of groups, which together define both the group rib update and the new cluster
    -- the second list contributes to the new group

    -- now we have the hashes for the affected clusters matched up with the prefixes involved
    -- look up the actual cluster values in clusterList and pair up with their update sets:
    targets :: [(Cluster,PrefixList)]
    targets = map (\(hash,pfxs) -> (fromJust $ Map.lookup hash (clusterList s),pfxs)) clusterMap

    -- The signature of clusterUpdate is: [(Cluster,PrefixList)] -> (Cluster,[CompositeGroup])
    -- Note: the output [CompositeGroup] update list is a subset of the CompositeGroups in the new Cluster, so a suboptimal strategy is to update every CompositeGroup present in the new Cluster.
    updateCluster :: (Cluster,PrefixList) -> (Cluster,[CompositeGroup])
    updateCluster _ = (emptyCluster,[]) -- todo
        where

        updateBasicGroups :: [BasicGroup] -> [Prefix] -> ([BasicGroup],[(BasicGroup, BasicGroup, BasicGroup)])
        updateBasicGroups _ _ = ([],[])

        updateCompositeGroup :: [(BasicGroup,BasicGroup,BasicGroup)] -> CompositeGroup -> CompositeGroup
        updateCompositeGroup _ _ = newCompositeGroup []

        updateCompositeGroups :: [(BasicGroup,BasicGroup,BasicGroup)] -> [CompositeGroup] -> [CompositeGroup]

    updateClusters :: [(Cluster,PrefixList)] -> (Cluster,[CompositeGroup])
    updateClusters ax = (mergeClusters cls, concat cgs) where
        (cls,cgs) = foldl (\(accb,acca) (a,b) -> (a:acca, b:accb)) ([],[]) ax

    -- this marks the completeion of the first stage
    (tmpCluster,newCompositeGroups) = updateClusters targets
    markedClusters = map fst targets
    newBasicGroup = mkBasicGroup unmatchedPrefixList

    -- build the new cluster
    newCluster = Cluster (unmatchedPrefixList ++ clPrefixes tmpCluster) (newCompositeGroup : clCompositeGroups tmpCluster) (newBasicGroup : clBasicGroups tmpCluster)

    -- now update the RIB state
    clusterHash
    clusterList = updateClusterList markedClusters newCluster (clusterList s)
    groupRib = updateGroupRib newCompositeGroups (groupRib s)
    prefixRib = updatePrefixRib  (prefixRib s)

displayState :: State -> String
displayState State{..} = "clusters: " ++ show (length clusters)
                       ++ " groups: " ++ show (length groups)
                       ++ " prefixes: " ++ show (length prefixes)
