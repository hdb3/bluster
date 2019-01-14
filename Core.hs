module Core where

import Data.Word
import Data.Hashable 

newtype Prefix = Prefix { fromPrefix :: Int } deriving Show
newtype GroupHash = GroupHash { fromGroupHash :: Int } deriving Show
--newtype ClusterHash = ClusterHash { fromClusterHash :: Word64 }
type PrefixList = [Prefix]
groupHash :: PrefixList -> GroupHash
groupHash pfxs = GroupHash $ Data.Hashable.hash $ map fromPrefix pfxs

data Cluster = Cluster { prefixes :: PrefixList
                       , groups :: [ Group ]
                       } deriving Show

data Group = Composite { compositeGroups :: [ Group ] }
           | Basic { basicPrefixes :: PrefixList } deriving Show

