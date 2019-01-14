module Containers where
import Data.IntMap.Strict as Map

import Core

type ClusterList = Map.IntMap Cluster
type PrefixRib = Map.IntMap GroupHash
type GroupRib = Map.IntMap Group

newPrefixRib :: PrefixRib
newPrefixRib = Map.empty

insertPrefixRib :: (Prefix,GroupHash) -> PrefixRib -> PrefixRib
insertPrefixRib _ _ = Map.empty

newGroupRib :: GroupRib
newGroupRib = Map.empty

insertGroupRib :: (Group,GroupHash) -> GroupRib -> GroupRib
insertGroupRib _ _ = Map.empty

newClusterList :: ClusterList
newClusterList = Map.empty

insertClusterList :: Cluster -> ClusterList -> ClusterList
insertClusterList _ _ = Map.empty

deleteClusterList :: GroupHash -> ClusterList
deleteClusterList _ = Map.empty
