module Annealing(optimizePath) where

import System.Random
import Data.List

type Graph = [[Float]]

swapNodes :: Int -> Int -> [Int] -> [Int]
swapNodes s1 s2 l = map (\x -> if x==s1 then s2 else if x==s2 then s1 else x) l

edgeCost :: Graph -> (Int, Int) -> Float
edgeCost g (from, to) = g !! from !! to


getNeighbours :: Int -> Int -> (Int, Int, Int)
getNeighbours current n | current == n-1 = (n-2, 0, 1)
                        | current == n-2 = (n-3, n-1, 0)
                        | current == 0 = (n-1, 1, 2)
                        | otherwise =  (current-1, current+1, current+2)


updatePath :: Graph -> [Int] -> Int -> Int -> Float -> Float -> Maybe [Int]
updatePath g path current nodeCount temp threshold  | delta > 0 || (temp >= 0.001 && (exp (-delta)/temp) >= threshold) = updated_path
                                    		        | otherwise = Nothing
                                                        where
                                                            (prev, next, next_next) = getNeighbours current nodeCount
                                                            old_cost = (edgeCost g (prev, current)) + (edgeCost g (next, next_next))
                                                            new_cost = (edgeCost g (prev, next)) + (edgeCost g (current, next_next))
                                                            delta = old_cost - new_cost
                                                            updated_path = Just (swapNodes current next path)
                                        
optimizePath :: Graph -> [Int] -> Int -> Int -> Float -> IO (Float, [Int])
optimizePath _ path _ 0  _ = return (0, path)
optimizePath g path nodeCount it temp = do
                                current <- randomRIO (0, nodeCount-1)
                                threshold <- randomRIO(0, 1.0)
                                print(threshold)
                                let result = updatePath g path current nodeCount temp threshold; temp_new = temp * 0.99
                                case result of
                                    Just path_new -> optimizePath g path_new nodeCount 100 temp_new
                                    Nothing -> optimizePath g path nodeCount (it-1) temp_new

genInitialPath :: Int -> IO [Int]
genInitialPath nodeCount = do
                            let nodes = [0..nodeCount-1]
                            return nodes
        
simulatedAnnealing :: Graph -> IO ()
simulatedAnnealing g = do
                        let nodeCount = length(g)
                        p <- genInitialPath nodeCount
                        path <- optimizePath g p nodeCount 100 1
                        print(path)
