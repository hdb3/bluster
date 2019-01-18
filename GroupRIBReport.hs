module GroupRIBReport where

import Core
import Containers
import Operations
import Consistency
import Analysis

groupRIBReport pfxs = do
    putStrLn "groupRIBReport"
    let st = mapRu newState pfxs
    --putStrLn $ "input prefix groups: " ++ show pfxs
    putStrLn $ "input prefix groups: " ++ show (length pfxs)
    putStrLn $ yellow $ displayState st
    --putStrLn $ "clusters:\n" ++ ( unlines $ map show $ clusters st )
    --putStrLn $ "groups:\n" ++ ( unlines $ map show $ groups st )
    maybe (putStrLn $ green "Consistency check pass") (\s -> putStrLn $ red $ "Consistency check fail: " ++ s) (consistency st)
    putStrLn $ yellow $ "analysis\n" ++ analysis st
    putStrLn "GroupRIBReport done"

    where

    yellow s = "\x001b[33m" ++ s ++ "\x001b[0m"
    green s = "\x001b[32m" ++ s ++ "\x001b[0m"
    red   s = "\x001b[31m" ++ s ++ "\x001b[0m"

    ru pl s = ribUpdate (map Prefix pl) s
    mapRu s [] = s
    mapRu s (a:ax) = mapRu (ru a s) ax

