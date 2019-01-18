module Main where

import Core
import Containers
import Operations

ru pl s = ribUpdate (map Prefix pl) s
mapRu s [] = s
mapRu s (a:ax) = mapRu (ru a s) ax

main = do
   putStrLn "Operations-test"
   try [[1]]
   --try [[1,2]]
   --try [[1,2] , [1,2,3] , [1,2,3,4] , [5] ]
   putStrLn "done"

try pfxs = do
    let s = mapRu newState pfxs
    print pfxs
    putStrLn $ displayState s
    print $ clusters s
