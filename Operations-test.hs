module Main where

import Core
import Containers
import Operations

main = do
   putStrLn "Operations-test"
   let s = newState
   print s
   putStrLn $ displayState s
   let s1 = insertGroup (map Prefix [1,2]) s
   print s1
   putStrLn $ displayState s1

   putStrLn "done"
