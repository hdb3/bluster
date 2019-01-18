module Main where

import Core
import Containers
import Operations

ru pl s = ribUpdate (map Prefix pl) s
mapRu s [] = s
mapRu s (a:ax) = mapRu (ru a s) ax

main = do
   putStrLn "Operations-test"
   --try [[1]]
   --try [[1,2]]
   --try [[1],[2]]
   try [[1],[1,2]]
   try [[1,2],[1]]
   --try [[1,2],[1],[2]]
   --try [[1,2] , [1,2,3] , [1,2,3,4] , [5] ]
   putStrLn "done"

try pfxs = do
    let s = mapRu newState pfxs
    putStrLn ""
    putStrLn $ "input prefix groups: " ++ show pfxs
    putStrLn $ displayState s
    putStrLn $ "clusters:\n" ++ ( unlines $ map show $ clusters s )
    putStrLn $ "groups:\n" ++ ( unlines $ map show $ groups s )
