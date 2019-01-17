module BasicOperations where
import Data.Maybe(isJust,fromJust)
import Data.List(sort,nub,foldl',intersect,(\\))

import Core
import Containers

updateBasicGroups :: [BasicGroup] -> [Prefix] -> ([BasicGroup],CompositeGroup,[(BasicGroup, BasicGroup, BasicGroup)])
updateBasicGroups bgs pl | null pl = error "updateBasicGroups should not be called on a null prefix list"
                         | otherwise = (newBasicGroups,newCompositeGroup,editList)
    where
    editListTmp = map partition bgs
    partition x = (x,inc,exc) where
        inc = (basicPrefixes x) `intersect` pl
        exc = (basicPrefixes x) \\ inc
    (incs,excs) = foldl' (\(acca,accb) (_,a,b) -> (a:acca,b:accb)) ([],[]) editListTmp
    editList = map (\(a,b,c) -> (a,mkBasicGroup b, mkBasicGroup c)) $ filter (\(_,a,b) -> not (null a) && not (null b)) editListTmp
    newBasicGroups = map mkBasicGroup $ filter ( not . null) $ foldl' (\ax (_,b,c) -> b:c:ax) [] editListTmp
    newCompositeGroup = mkCompositeGroup $ map mkBasicGroup $ filter ( not . null ) incs

updateCompositeGroup :: [(BasicGroup,BasicGroup,BasicGroup)] -> CompositeGroup -> CompositeGroup
updateCompositeGroup _ _ = mkCompositeGroup [] -- todo
