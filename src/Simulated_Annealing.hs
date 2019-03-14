module Simulated_Annealing(simulatedAnnealing, pathCost, genInitialPath, getNeighbours, swapNodes, calcDelta) where
import System.Random
import Data.List

pathCost :: [[Float]] -> [Int] -> Int -> Float
pathCost g path nc = foldr (+) 0.0 edge_costs
            where
                edges = zip path ((tail path) ++ [head path])
                edge_costs = map (\tf -> edgeCost g tf) edges

-- Dado quatro pontos, o caminho, e as distâncias entre os pontos do caminho, calcula o
-- delta
calcDelta :: (Int,Int,Int,Int) -> [Int] -> [[Float]] -> Float
calcDelta (c,prev,n1,n2) route ll = delta
                                where
                                    pC = route !! c
                                    pPrev = route !! prev
                                    pN1 = route !! n1
                                    pN2 = route !! n2
                                    ll0 = (ll !! pPrev) !! pN1
                                    ll1 = (ll !! pC) !! pN2
                                    ll2 = (ll !! pPrev) !! pC
                                    ll3 = (ll !! pN1) !! pN2
                                    delta = (ll0 + ll1) - (ll2 + ll3)

swapNodes :: Int -> Int -> [Int] -> [Int]
swapNodes s1 s2 l = map (\x -> if x==s1 then s2 else if x==s2 then s1 else x) l

edgeCost :: [[Float]] -> (Int, Int) -> Float
edgeCost g (from, to) = (g !! from) !! to

-- Dado um ponto c e o número de pontos possíveis, calcula os vizinhos de c
getNeighbours :: Int -> Int -> (Int, Int, Int)
getNeighbours current n | current == n-1 = (n-2, 0, 1)
                        | current == n-2 = (n-3, n-1, 0)
                        | current == 0 = (n-1, 1, 2)
                        | otherwise =  (current-1, current+1, current+2)


updatePath :: [[Float]] -> [Int] -> Int -> Int -> Float -> Float -> Maybe [Int]
updatePath g path current nodeCount temp threshold  | delta < 0 || (temp > 0.001 && (exp ((-delta)/temp)) >= threshold) = updated_path
                                                    | otherwise = Nothing
                                                        where
                                                            (prev, next, next_next) = getNeighbours current nodeCount
                                                            delta = calcDelta (current, prev, next, next_next) path g
                                                            updated_path = Just (swapNodes (path !! current) (path !! next) path)
                                        
optimizePath :: [[Float]] -> [Int] -> Int -> Int -> Int -> Float -> IO (Float, [Int])
optimizePath g path nc _ 0 _ = return (pathCost g path nc, path)
optimizePath g path nc tot_it it_left temp = do
                                current <- randomRIO (0, nc-1)
                                threshold <- randomRIO (0, 1.0)
                                let result = updatePath g path current nc temp threshold; temp_new = temp * 0.999
                                case result of
                                    Just path_new -> optimizePath g path_new nc tot_it tot_it temp_new
                                    Nothing -> optimizePath g path nc tot_it (it_left-1) temp_new

genInitialPath :: [Int] -> Int -> IO [Int]
genInitialPath [] _ = return []
genInitialPath nodes nc = do
                        n_head <- randomRIO(0, nc-1)
                        let nodes_n = swapNodes (nodes !! 0) (nodes !! n_head) nodes
                        nodes_r <- genInitialPath (tail nodes_n) (nc-1)
                        return ((nodes_n !! 0) : nodes_r)
        
simulatedAnnealing :: [[Float]] -> Int -> IO (Float, [Int])
simulatedAnnealing g it = do
                        let nc = length(g); nodes = [0..nc-1]
                        p <- genInitialPath nodes nc
                        optimizePath g p nc it it 1
