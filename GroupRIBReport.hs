module GroupRIBReport where

import Core
import Operations
import Consistency
import Analysis

groupRIBReport :: [[Int]] -> String
groupRIBReport pfxs =
    "groupRIBReport"
    ++ "\ninput prefix groups: " ++ show (length pfxs)
    ++ "\n" ++ ( yellow $ displayState st)
    ++ "\n" ++ ( maybe (green "Consistency check pass") (\s -> red $ "Consistency check fail: " ++ s) (consistency st) )
    ++ "\n" ++ ( yellow $ "analysis\n" ++ analysis st)
    ++ "\nGroupRIBReport done"

    where

    st = mapRu newState pfxs
    yellow s = "\x001b[33m" ++ s ++ "\x001b[0m"
    green s = "\x001b[32m" ++ s ++ "\x001b[0m"
    red   s = "\x001b[31m" ++ s ++ "\x001b[0m"

    ru pl = ribUpdate (map Prefix pl)
    mapRu = foldl (flip ru)
