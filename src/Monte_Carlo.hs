module Monte_Carlo(traveling_monte_carlo) where
import Simulated_Annealing (genInitialPath, pathCost, getNeighbours)

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

swapNodes :: Int -> Int -> [Int] -> [Int]
swapNodes s1 s2 l = map (\x -> if x==s1 then s2 else if x==s2 then s1 else x) l

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
