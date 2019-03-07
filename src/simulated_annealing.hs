module Annealing(optimizePath) where

-- import System.Random

type Graph = [[Float]]

edgeCost :: Graph -> Int -> Int -> Float
edgeCost g from to = g !! from !! to

-- TODO: convert to Maybe [Float]
updatePath :: Graph -> [Int] -> Float -> (Bool, [Int])
updatePath g path temp  | delta > 0 && (exp (-delta)/temp) > 1  = (True, (prev:next:current:next_next:t))
                        | otherwise = (False, path)
                                where
                                    (prev:current:next:next_next:t) = path
                                    old_cost = (edgeCost g prev current) + (edgeCost g next next_next)
                                    new_cost = (edgeCost g prev next) + (edgeCost g current next_next)
                                    delta = old_cost - new_cost
                                        
optimizePath :: Graph -> [Int] -> Int -> Float -> [Int]
optimizePath _ path 0  _ = path
optimizePath g path it  temp    | (fst path_new) = optimizePath g (snd path_new) 100 temp_new
                                | otherwise = optimizePath g path (it-1) temp_new
                                    where 
                                        path_new = updatePath g path temp
                                        temp_new = temp * 0.999