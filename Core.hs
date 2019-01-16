{-# LANGUAGE FlexibleInstances,DeriveGeneric #-}
module Core where

import Data.Word
import Data.Hashable 
import Data.List(sort)
import GHC.Generics(Generic)

newtype Prefix = Prefix { fromPrefix :: Int } deriving (Eq,Ord,Show,Generic)
newtype Hash = Hash { fromHash :: Int } deriving (Eq,Ord,Show,Generic)
type PrefixList = [Prefix]

data Cluster = Cluster { clHash :: Hash
                       , clPrefixes :: PrefixList -- redundant since the prefix list is also contained in the Basic groups
                       , clCompositeGroups :: [CompositeGroup]
                       , clBasicGroups :: [ BasicGroup ]
                       } deriving Show

data BasicGroup = BasicGroup { bgHash :: Hash , basicPrefixes :: PrefixList } deriving (Show,Ord,Generic)

instance Eq BasicGroup where
    (==) bg1 bg2 = (bgHash bg1 == bgHash bg2)

data CompositeGroup = CompositeGroup { cgHash :: Hash , compositeGroups :: [ BasicGroup ] } deriving (Show,Generic)

instance {-# INCOHERENT #-} Hashable [Prefix] where
    hashWithSalt s pl = hashWithSalt s $ sort $ map fromPrefix pl

instance Hashable Prefix
instance Hashable CompositeGroup
instance Hashable BasicGroup
instance Hashable Hash

mkBasicGroup :: PrefixList -> BasicGroup
mkBasicGroup pl = BasicGroup (Hash $ Data.Hashable.hash $ sort pl) (sort pl)

mkCompositeGroup :: [BasicGroup] -> CompositeGroup
mkCompositeGroup bgs = CompositeGroup (Hash $ Data.Hashable.hash (sort bgs)) (sort bgs)

mkCluster :: PrefixList -> [CompositeGroup] -> [ BasicGroup ] -> Cluster
mkCluster a b c = Cluster (Hash $ Data.Hashable.hash (a,b,c)) a b c

emptyCluster = mkCluster [] [] [] 

mergeClusters :: [Cluster] -> Cluster
mergeClusters = foldl (\(Cluster _ acca accb accc) (Cluster _ xa xb xc) -> (mkCluster (acca++xa) (accb++xb) (accc++xc))) emptyCluster
