module Operations where

import Core
import Containers

data State = State { clusters :: ClusterList
                   , groups :: GroupRib
                   , prefixes :: PrefixRib
                   } deriving Show

newState = State newClusterList newGroupRib newPrefixRib

insertGroup :: PrefixList -> State -> State
insertGroup _ a = a

