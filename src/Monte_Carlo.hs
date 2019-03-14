module Monte_Carlo(traveling_monte_carlo) where

import System.Random
import Data.List

-- Recebe uma Matriz de floats com os custos entre os pontos (indices)
-- Devolve o caminho (lista de inteiros) e o custo total (float)
traveling_monte_carlo :: [[Float]] -> IO (Float,[Int])
traveling_monte_carlo [] = return (0,[])
traveling_monte_carlo (hL:t) = do
                                let n = length hL
                                let perm = permutations [0..n-1]
                                randValue <- randomRIO (0, (length perm)-1)
                                let route = perm !! randValue
                                let pathLastToFirst = ((hL:t) !! last route) !! head route
                                let dist = calcDist route (hL:t) pathLastToFirst
                                retValue <- iterations (hL:t) n route dist 100 100
                                return retValue

genInitialPath :: [Int] -> Int -> IO [Int]
genInitialPath [] _ = return []
genInitialPath nodes nc = do
                        n_head <- randomRIO(0, nc-1)
                        let nodes_n = swapNodes (nodes !! 0) (nodes !! n_head) nodes
                        nodes_r <- genInitialPath (tail nodes_n) (nc-1)
                        return ((nodes_n !! 0) : nodes_r)

swapNodes :: Int -> Int -> [Int] -> [Int]
swapNodes s1 s2 l = map (\x -> if x==s1 then s2 else if x==s2 then s1 else x) l

-- Recebe uma lista com o caminho a percorrer e recebe as distâncias entre cada ponto (matriz)
-- Recebe também um valor inicial
-- Percorre o caminho e vai somando as distâncias entre cada dois pontos do caminho, o atual e
-- o seguinte. Esta função não soma o valor entre o ultimo e o primeiro ponto do caminho, algo
-- que tem de ser feito antes e passado como argumento (valor inicial) ou começar com valor 
-- inicial igual a 0 e somar no fim da chamada desta função
calcDist :: Num a => [Int] -> [[a]] -> a -> a
calcDist _ [] _ = 0
calcDist [] _ _ = 0
calcDist [a] ll dist = dist
calcDist (p:t) ll dist = calcDist t ll (dist+path)
                            where
                                path = (ll !! p) !! (head t)

-- Recebe as distâncias entre os pontos, o grau da matriz, o caminho inicial, o custo inicial,
-- e o número de iteração atual e o número de iterações inicial
-- Em cada iteração realiza random de um valor, calcula os seus vizinhos, calcula o delta, e caso
-- o delta seja menor que 0 atualiza a rota, o custo(dist), caso o delta maior que 0 atualiza a
-- iteração
iterations :: [[Float]] -> Int -> [Int] -> Float -> Int -> Int -> IO (Float,[Int])
iterations _ _ route dist 0 _ = return (dist,route)
iterations ll n route dist it itStart = do
                                c <- randomRIO (0,n-1)
                                let (prev,n1,n2) = getNeighbors c (n-1)
                                let delta = calcDelta (c,prev,n1,n2) route ll
                                if delta < 0
                                    then do
                                        retValue <- iterations ll n (swapNodes c n1 route) (dist+delta) itStart itStart
                                        return retValue
                                    else do
                                        retValue <- iterations ll n route dist (it-1) itStart
                                        return retValue

-- Dado um ponto c e o número de pontos possíveis, calcula os vizinhos de c
getNeighbors :: Int -> Int -> (Int,Int,Int)
getNeighbors 0 n = (n,1,2)
getNeighbors c n
        | c==n-1 = (n-2,n,0)
        | c==n = (n-1,0,1)
        | otherwise = (c-1,c+1,c+2)

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
