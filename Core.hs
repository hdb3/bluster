module Core where

import Data.Word
import Data.Hashable 
import Data.List(sort)

newtype Prefix = Prefix { fromPrefix :: Int } deriving (Eq,Ord,Show)
newtype GroupHash = GroupHash { fromGroupHash :: Int } deriving (Eq,Ord,Show)
--newtype ClusterHash = ClusterHash { fromClusterHash :: Word64 }
type PrefixList = [Prefix]
groupHash :: PrefixList -> GroupHash
groupHash pfxs = GroupHash $ Data.Hashable.hash $ sort $ map fromPrefix pfxs

data Cluster = Cluster { clPrefixes :: PrefixList -- redundant since the prefix lisst is also contained in the Basic groups
                       , clCompositeGroups :: [CompositeGroup]
                       , clBasicGroups :: [ BasicGroup ]
                       } deriving Show

data BasicGroup = BasicGroup { bgHash :: GroupHash , basicPrefixes :: PrefixList } deriving Show
instance Eq BasicGroup where
    (==) bg1 bg2 = (bgHash bg1 == bgHash bg2)

data CompositeGroup = CompositeGroup { cgHash :: GroupHash , compositeGroups :: [ BasicGroup ] } deriving Show

newCluster = Cluster [] [] []
newBasicGroup :: PrefixList -> BasicGroup
newBasicGroup pl = BasicGroup (groupHash pl) pl

newCompositeGroup :: [BasicGroup] -> CompositeGroup
newCompositeGroup bgs = CompositeGroup (groupHash (concatMap basicPrefixes bgs)) bgs
