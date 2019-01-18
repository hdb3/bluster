module BasicOperations where
--import Data.Maybe(isJust,fromJust)
import Data.List(foldl',intersect,(\\))

import Core
--import Containers

updateBasicGroups :: [BasicGroup] -> [Prefix] -> ([BasicGroup],CompositeGroup,[(BasicGroup, BasicGroup, BasicGroup)])
updateBasicGroups bgs pl | null pl = error "updateBasicGroups should not be called on a null prefix list"
                         | otherwise = (newBasicGroups,newCompositeGroup,editList)
    where
    editListTmp = map partition bgs
    partition x = (x,inc,exc) where
        inc = (basicPrefixes x) `intersect` pl
        exc = (basicPrefixes x) \\ inc
    --(incs,excs) = foldl' (\(acca,accb) (_,a,b) -> (a:acca,b:accb)) ([],[]) editListTmp
    incs = map (\(_,x,_) -> x) editListTmp
    editList = map (\(a,b,c) -> (a,mkBasicGroup b, mkBasicGroup c)) $ filter (\(_,a,b) -> not (null a) && not (null b)) editListTmp
    newBasicGroups = map mkBasicGroup $ filter ( not . null) $ foldl' (\ax (_,b,c) -> b:c:ax) [] editListTmp
    newCompositeGroup = mkCompositeGroup $ map mkBasicGroup $ filter ( not . null ) incs

updateCompositeGroup :: [(BasicGroup,BasicGroup,BasicGroup)] -> CompositeGroup -> CompositeGroup
--updateCompositeGroup _ _ = mkCompositeGroup [] -- todo

{-
the premise of updateCompositeGroup is that one or more members of an existing composite group
must be replaced by a pair of new ones - simples....
this requires comapring each member of the CG with each head of the edit list
since we know tha both lists are unique, a first match completes the search for both candidates
if it were given that the lists are sorted, then the process could be even more optimised....
-}

updateCompositeGroup el cg = mkCompositeGroup $ go1 el $ compositeGroups cg
    where
    go1 :: [(BasicGroup,BasicGroup,BasicGroup)] -> [ BasicGroup ] -> [ BasicGroup ] 
    go1 el1 = concatMap (go2 el1)
    go2 :: [(BasicGroup,BasicGroup,BasicGroup)] -> BasicGroup -> [ BasicGroup ]
    go2 [] bg = [bg]
    go2 ( (hd,bg1,bg2) : el2 ) bg0 | hd == bg0 = [bg1,bg2]
                                   | otherwise = go2 el2 bg0

updateCompositeGroups :: [(BasicGroup,BasicGroup,BasicGroup)] -> [CompositeGroup] -> [CompositeGroup]
updateCompositeGroups updates = map (updateCompositeGroup updates)
