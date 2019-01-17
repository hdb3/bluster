module Containers where
import qualified Data.IntMap.Strict as Map
import Data.List(foldl')

import Core

type ClusterList = Map.IntMap Cluster
type PrefixRib = Map.IntMap Hash
type GroupRib = Map.IntMap CompositeGroup

newPrefixRib :: PrefixRib
newPrefixRib = Map.empty

insertPrefixRib :: (Prefix,Hash) -> PrefixRib -> PrefixRib
insertPrefixRib _ _ = Map.empty

newGroupRib :: GroupRib
newGroupRib = Map.empty

insertGroupRib :: CompositeGroup -> GroupRib -> GroupRib
insertGroupRib _ _ = Map.empty

newClusterList :: ClusterList
newClusterList = Map.empty

insertClusterList :: Cluster -> ClusterList -> ClusterList
insertClusterList _ _ = Map.empty

deleteClusterList :: Hash -> ClusterList
deleteClusterList _ = Map.empty

{-
updateClusterList newCluster markedClusters oldClusterList = Map.insert (fromHash $ clHash newCluster)  newCluster tmpClusterList
    where
    tmpClusterList :: ClusterList
    tmpClusterList = foldl' (\cl c -> Map.delete (fromHash $ clHash c) cl ) oldClusterList markedClusters
-}

updateClusterList :: Cluster -> [Cluster] -> ClusterList -> ClusterList
updateClusterList newCluster markedClusters oldClusterList = Map.insert (fromHash $ clHash newCluster) newCluster
                                                            $ foldl' (\cl c -> Map.delete (fromHash $ clHash c) cl ) oldClusterList markedClusters

updateGroupRib :: [CompositeGroup] -> GroupRib -> GroupRib 
updateGroupRib cgs oldRib = foldl' (\rib cg -> Map.insert (fromHash $ cgHash cg) cg rib ) oldRib cgs

updatePrefixRib :: PrefixList -> Cluster -> PrefixRib -> PrefixRib
updatePrefixRib pfxs cl oldRib = foldl' (\rib pfx -> Map.insert (fromPrefix pfx) (clHash cl) rib ) oldRib pfxs

