module Simulated_Annealing(simulatedAnnealing, pathCost) where

import System.Random
import Data.List

type Graph = [[Float]]

pathCost :: Graph -> [Int] -> Int -> Float
pathCost g path nc = foldr (+) 0.0 edge_costs
            where
                edges = zip path ((tail path) ++ [head path])
                edge_costs = map (\tf -> edgeCost g tf) edges


swapNodes :: Int -> Int -> [Int] -> [Int]
swapNodes s1 s2 l = map (\x -> if x==s1 then s2 else if x==s2 then s1 else x) l

edgeCost :: Graph -> (Int, Int) -> Float
edgeCost g (from, to) = (g !! from) !! to


getNeighbours :: Int -> Int -> (Int, Int, Int)
getNeighbours current n | current == n-1 = (n-2, 0, 1)
                        | current == n-2 = (n-3, n-1, 0)
                        | current == 0 = (n-1, 1, 2)
                        | otherwise =  (current-1, current+1, current+2)


updatePath :: Graph -> [Int] -> Int -> Int -> Float -> Float -> Maybe [Int]
updatePath g path current nodeCount temp threshold  | delta < 0 || (temp > 0.001 && (exp ((-delta)/temp)) >= threshold) = updated_path
                                                    | otherwise = Nothing
                                                        where
                                                            (prev, next, next_next) = getNeighbours current nodeCount
                                                            old_cost = (edgeCost g (path !! prev, path !! current)) + (edgeCost g (path !! next,path !! next_next))
                                                            new_cost = (edgeCost g (path !! prev, path !! next)) + (edgeCost g (path !! current, path !! next_next)) 
                                                            delta = new_cost - old_cost
                                                            updated_path = Just (swapNodes current next path)
                                        
optimizePath :: Graph -> [Int] -> Int -> Int -> Float -> IO (Float, [Int])
optimizePath g path nc 0  _ = return (pathCost g path nc, path)
optimizePath g path nc it temp = do
                                current <- randomRIO (0, nc-1)
                                threshold <- randomRIO (0, 1.0)
                                let result = updatePath g path current nc temp threshold; temp_new = temp * 0.999
                                case result of
                                    Just path_new -> optimizePath g path_new nc 100 temp_new
                                    Nothing -> optimizePath g path nc (it-1) temp_new

genInitialPath :: [Int] -> Int -> IO [Int]
genInitialPath [] _ = return []
genInitialPath nodes nc = do
                        n_head <- randomRIO(0, nc-1)
                        let nodes_n = swapNodes (nodes !! 0) (nodes !! n_head) nodes
                        nodes_r <- genInitialPath (tail nodes_n) (nc-1)
                        return ((nodes_n !! 0) : nodes_r)
        
simulatedAnnealing :: Graph -> IO (Float, [Int])
simulatedAnnealing g = do
                        let nc = length(g); nodes = [0..nc-1]
                        p <- genInitialPath nodes nc
                        optimizePath g p nc 100 1
