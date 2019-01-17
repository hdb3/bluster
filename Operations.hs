{-# LANGUAGE RecordWildCards #-}
module Operations where
import Data.Maybe(isJust,fromJust)
import Data.List(sort,nub,foldl')

import Core
import Containers

data State = State { clusterList :: ClusterList
                   , groupRib :: GroupRib
                   , prefixRib :: PrefixRib
                   } deriving Show

newState = State emptyClusterList emptyGroupRib emptyPrefixRib

ribUpdate :: PrefixList -> State -> State
--insertGroup _ a = a
--ribUpdate pl s = if present then s else State{..} where
ribUpdate pl s = if present then error "trying to insert an existing group" else State newClusterList newGroupRib newPrefixRib
    where

    -- sanity check only .....
    present = isJust (Containers.lookup (prefixListHash pl) (groupRib s) )

    (clusterMap,unmatchedPrefixList) = getClusterMap pl (prefixRib s)
    -- clusterMap = getClusterMap pl prefixes -- using RecordWildCards

    getClusterMap :: PrefixList -> PrefixRib -> ( [(Hash,PrefixList)] , PrefixList )
    -- first value returns prefixes mapped already, second value is unmapped prefixes
    getClusterMap pl pr = (rollUp matched, sort unmatched)
        where
        --r = map (\pfx -> (pfx,Map.lookup pfx pr)) pl
        --(matched,unmatched) = foldl (\(m,u) t -> if isNothing (snd t) then (m,t:u) else (t:m,u)) ([],[]) r
        (matched,unmatched) = foldl' (\(m,u) pfx -> maybe (m,pfx:u)
                                                         (\g -> ((g,pfx):m,u))
                                                         (Containers.lookup (prefixHash pfx) pr )
                                    )
                                    ([],[])
                                    pl
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
    targets = map (\(hash,pfxs) -> (fromJust $ Containers.lookup hash (clusterList s),pfxs)) clusterMap

    -- The signature of clusterUpdate is: [(Cluster,PrefixList)] -> (Cluster,[CompositeGroup])
    -- Note: the output [CompositeGroup] update list is a subset of the CompositeGroups in the new Cluster, so a suboptimal strategy is to update every CompositeGroup present in the new Cluster.

    updateCluster :: (Cluster,PrefixList) -> (Cluster,CompositeGroup)
    updateCluster _ = (emptyCluster,emptyCompositeGroup) -- todo
        where

        updateBasicGroups :: [BasicGroup] -> [Prefix] -> ([BasicGroup],[(BasicGroup, BasicGroup, BasicGroup)])
        updateBasicGroups _ _ = ([],[]) -- todo

        updateCompositeGroup :: [(BasicGroup,BasicGroup,BasicGroup)] -> CompositeGroup -> CompositeGroup
        updateCompositeGroup _ _ = mkCompositeGroup [] -- todo

        updateCompositeGroups :: [(BasicGroup,BasicGroup,BasicGroup)] -> [CompositeGroup] -> [CompositeGroup]
        updateCompositeGroups updates = map (updateCompositeGroup updates)

    updateClusters :: [(Cluster,PrefixList)] -> (Cluster,CompositeGroup)
    updateClusters ax = (mergeClusters cls, mergeCompositeGroups cgs) where
        (cls,cgs) = mergeUpdates $ map updateCluster ax

        mergeUpdates :: [(Cluster,CompositeGroup)] -> ([Cluster],[CompositeGroup])
        mergeUpdates = foldl' (\(acca,accb) (a,b) -> (a:acca, b:accb)) ([],[])

    -- this marks the completeion of the first stage
    (tmpCluster,newCompositeGroup) = updateClusters targets
    markedClusters = map fst targets
    newBasicGroup = mkBasicGroup unmatchedPrefixList

    -- build the new cluster
    newCluster = mkCluster (unmatchedPrefixList ++ clPrefixes tmpCluster) (newCompositeGroup : clCompositeGroups tmpCluster) (newBasicGroup : clBasicGroups tmpCluster)

    -- now update the RIB state
    newClusterList = updateClusterList newCluster markedClusters (clusterList s)
    newGroupRib    = updateGroupRib (clCompositeGroups newCluster) (groupRib s)
    newPrefixRib   = updatePrefixRib newCluster (prefixRib s)

displayState :: State -> String
displayState State{..} = "clusters: " ++ show (length clusterList)
                       ++ " groups: " ++ show (length groupRib)
                       ++ " prefixes: " ++ show (length prefixRib)
