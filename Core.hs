{-# LANGUAGE OverloadedStrings,FlexibleInstances,DeriveGeneric #-}
module Core where

--import Data.Word
import Data.Hashable 
import Data.List(sort)
import GHC.Generics(Generic)

newtype Prefix = Prefix { fromPrefix :: Int } deriving (Eq,Ord,Generic)
instance Show Prefix where
    show = show . fromPrefix

instance Num Prefix where
    fromInteger x = Prefix $ fromIntegral x

newtype Hash = Hash { fromHash :: Int } deriving (Eq,Ord,Show,Generic)
type PrefixList = [Prefix]

data Cluster = Cluster { clHash :: Hash
                       , clPrefixes :: PrefixList -- redundant since the prefix list is also contained in the Basic groups
                       , clCompositeGroups :: [CompositeGroup]
                       , clBasicGroups :: [ BasicGroup ]
                       } deriving Show

data BasicGroup = BasicGroup { bgHash :: Hash , basicPrefixes :: PrefixList } deriving (Ord,Generic)
instance Show BasicGroup where
    show = show . basicPrefixes

instance Eq BasicGroup where
    (==) bg1 bg2 = bgHash bg1 == bgHash bg2

data CompositeGroup = CompositeGroup { cgHash :: Hash , compositeGroups :: [ BasicGroup ] } deriving Generic
instance Show CompositeGroup where
    show = show . compositeGroups

instance {-# INCOHERENT #-} Hashable [Prefix] where
    hashWithSalt s pl = hashWithSalt s $ sort $ map fromPrefix pl

instance Hashable Prefix
instance Hashable CompositeGroup
instance Hashable BasicGroup
instance Hashable Hash

prefixHash :: Prefix -> Hash
prefixHash = Hash . fromPrefix

prefixListHash :: PrefixList -> Hash
prefixListHash pl = Hash $ Data.Hashable.hash $ sort pl

mkBasicGroup :: PrefixList -> BasicGroup
mkBasicGroup pl = BasicGroup (prefixListHash pl) (sort pl)

mkCompositeGroup :: [BasicGroup] -> CompositeGroup
mkCompositeGroup bgs = CompositeGroup (Hash $ Data.Hashable.hash (sort bgs)) (sort bgs)

mkCluster :: PrefixList -> [CompositeGroup] -> [ BasicGroup ] -> Cluster
mkCluster a b c = Cluster (Hash $ Data.Hashable.hash (a,b,c)) a b c

emptyCluster :: Cluster
emptyCluster = mkCluster [] [] [] 

mergeClusters :: [Cluster] -> Cluster
mergeClusters = foldl (\(Cluster _ acca accb accc) (Cluster _ xa xb xc) -> (mkCluster (acca++xa) (accb++xb) (accc++xc))) emptyCluster

mergeCompositeGroups :: [CompositeGroup] -> CompositeGroup
mergeCompositeGroups = mkCompositeGroup . concatMap compositeGroups

emptyCompositeGroup :: CompositeGroup
emptyCompositeGroup = mkCompositeGroup [] 
