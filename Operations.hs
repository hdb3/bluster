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

insertGroup :: PrefixList -> State -> State
--insertGroup _ a = a
insertGroup pl s = if present then s else State{..} where
    hash = groupHash pl
    present = isJust (Map.lookup groups hash)
    clusterMap = getClusterMap pl (prefixRib s)
    -- clusterMap = getClusterMap pl prefixes -- using RecordWildCards
    getClusterMap :: PrefixList -> PrefixRib -> ([(GroupHash,[Prefix]),[Prefix])
    -- first value is prefixes mapped already, second value is unmapped prefixes
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

    -- now we have the affected clusters (and the prefixes involved)
    -- mtahc up the clusters with their update sets:
    targetClusters = map (\(hash,pfxs) -> (fromJust $ Map.lookup hash (clusterList s),pfxs)) clusterMap
    updateCluster :: PrefixList -> (GroupHash,[Prefix]) -> ([ Group ] , [ Group ] )
    -- cluster update requires first the generation of any new basic groups
    --     new basic groups are always binary partiotins of existing basic groups
    --     so the result of this operation is just a change set
    --     the composite groups are added to the cluster composite list, basic groups to the basic list
    --     old basic groups are discarded, and old composite groups must be updated where their members are affected by splitting former basic groups
    updateCluster _ _ = ([],[]) -- TODO....
    (updates,newGroup) = getClusterMap pl prefixes
    (axn,bxn) = foldl (\(accb,acca) (a,b) -> (a++acca, b++accb)) ([],[]) updates -- maybe a zippy function does this cleaner?
    newCompositeGroup = if null newGroup then axx else Basic newGroup : axx
    newGroups = 
    newCluster = Cluster 
    newPrefixList =

displayState :: State -> String
displayState State{..} = "clusters: " ++ show (length clusters)
                       ++ " groups: " ++ show (length groups)
                       ++ " prefixes: " ++ show (length prefixes)
