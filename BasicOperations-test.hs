module Main where
import Data.Maybe(isJust,fromJust)
import Data.List(sort,nub,foldl',intersect,(\\))

import Core
import Containers
import BasicOperations

--updateBasicGroups :: [BasicGroup] -> [Prefix] -> ([BasicGroup],CompositeGroup,[(BasicGroup, BasicGroup, BasicGroup)])

display :: ([BasicGroup],CompositeGroup,[(BasicGroup, BasicGroup, BasicGroup)]) -> IO()
display (newGroups , cg , editList) = do
    putStrLn $ "new groups:      " ++ show newGroups
    putStrLn $ "composite group: " ++ show cg
    putStrLn $ "edit list:       " ++ show editList

main = do
    putStrLn "BasicOperations-test"
    
    -- note - the only valid inputs are those in which the prefix list and basic groups are both not null
    --        and in which the prefix list and basic groups are all disjoint
    --        and in which all members of prefix list are members of the basic groups
    -- display $ updateBasicGroups [] [] -- INVALID REQUEST!!!!
    -- putStrLn ""
    -- display $ updateBasicGroups [(mkBasicGroup [1])] [] -- INVALID REQUEST!!!!
    putStrLn "1,2 : 2"
    display $ updateBasicGroups [(mkBasicGroup [1,2])] [2]
    putStrLn ""

    putStrLn "1,2 3,4 : 2"
    display $ updateBasicGroups (map mkBasicGroup [[1,2],[3,4]]) [2]
    putStrLn ""

    putStrLn "1,2 3,4 : 2,3"
    display $ updateBasicGroups (map mkBasicGroup [[1,2],[3,4]]) [2,3]
    putStrLn ""

    putStrLn "1,2,3,4  5 : 2,3,5"
    display $ updateBasicGroups (map mkBasicGroup [[1,2,3,4],[5]]) [2,3,5]
    putStrLn ""

    putStrLn "1,2,3,4  5,6 7 : 2,3,5"
    display $ updateBasicGroups (map mkBasicGroup [[1,2,3,4],[5,6],[7]]) [2,3,5]
    putStrLn ""

    putStrLn "1,2,3,4  5,6 7 : 7,2,3,5"
    display $ updateBasicGroups (map mkBasicGroup [[1,2,3,4],[5,6],[7]]) [7,2,3,5]
    putStrLn ""
