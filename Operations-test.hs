module Main where

import Core
import Containers
import Operations

ru pl s = ribUpdate (map Prefix pl) s
mapRu s [] = s
mapRu s (a:ax) = mapRu (ru a s) ax

main = do
   putStrLn "Operations-test"
   let s0 = newState
   print s0
   putStrLn $ displayState s0
   let s1 = ru [1,2] s0
   print s1
   putStrLn $ displayState s1

   let plx = [[1,2] , [1,2,3] , [1,2,3,4] , [5] ]
       s' = mapRu newState plx
   print plx
   --let s' = ru [1,2] $ ru [1,2,3] $ ru [1,2,3,4] $ ru [5] newState
   print s'
   putStrLn $ displayState s'

   putStrLn "done"
