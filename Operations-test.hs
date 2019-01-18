module Main where

import Core
import Containers
import Operations
import Consistency
import Analysis

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
   try [[1,2] , [1,2,3] , [1,2,3,4] , [5] ]
   putStrLn "done"

try pfxs = do
    let st = mapRu newState pfxs
    putStrLn $ "input prefix groups: " ++ show pfxs
    putStrLn $ displayState st
    putStrLn $ "clusters:\n" ++ ( unlines $ map show $ clusters st )
    putStrLn $ "groups:\n" ++ ( unlines $ map show $ groups st )
    maybe (putStrLn $ green "Consistency check pass") (\s -> putStrLn $ red $ "Consistency check fail: " ++ s) (consistency st)
    putStrLn $ yellow $ "analysis\n" ++ analysis st

yellow s = "\x001b[33m" ++ s ++ "\x001b[0m"
green s = "\x001b[32m" ++ s ++ "\x001b[0m"
red   s = "\x001b[31m" ++ s ++ "\x001b[0m"
