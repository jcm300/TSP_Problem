module Monte_Carlo(traveling_monte_carlo) where
import Simulated_Annealing (genInitialPath, pathCost, getNeighbours, swapNodes, calcDelta)

import System.Random
import Data.List

-- Recebe uma Matriz de floats com os custos entre os pontos (indices)
-- Devolve o caminho (lista de inteiros) e o custo total (float)
traveling_monte_carlo :: [[Float]] -> Int -> IO (Float,[Int])
traveling_monte_carlo [] _ = return (0,[])
traveling_monte_carlo g it = do
                                let n = length g
                                route <- genInitialPath [0..n-1] n
                                let dist = pathCost g route n
                                retValue <- iterations g n route dist it it
                                return retValue

-- Recebe as distâncias entre os pontos, o grau da matriz, o caminho inicial, o custo inicial,
-- e o número de iteração atual e o número de iterações inicial
-- Em cada iteração realiza random de um valor, calcula os seus vizinhos, calcula o delta, e caso
-- o delta seja menor que 0 atualiza a rota, o custo(dist), caso o delta maior que 0 atualiza a
-- iteração
iterations :: [[Float]] -> Int -> [Int] -> Float -> Int -> Int -> IO (Float,[Int])
iterations _ _ route dist 0 _ = return (dist,route)
iterations graph n route dist it itStart = do
                                c <- randomRIO (0,n-1)
                                let (prev,n1,n2) = getNeighbours c n
                                let delta = calcDelta (c,prev,n1,n2) route graph
                                if delta < 0 then
                                    iterations graph n (swapNodes (route !! c) (route !! n1) route) (dist+delta) itStart itStart
                                else
                                    iterations graph n route dist (it-1) itStart
