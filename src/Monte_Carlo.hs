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
                                let routeUp = swap route delta (c,n1)
                                let distUp = updateDist dist delta
                                let itUp = incr it delta itStart
                                retValue <- iterations ll n routeUp distUp itUp itStart
                                return retValue

-- Dado um ponto c e o número de pontos possíveis, calcula os vizinhos de c
getNeighbors :: Int -> Int -> (Int,Int,Int)
getNeighbors 0 n = (n,1,2)
getNeighbors c n
        | c==n-1 = (n-2,n,0)
        | c==n = (n-1,0,1)
        | otherwise = (c-1,c+1,c+2)

-- Dado um delta e o número inicial de iterações, se delta for menor que 0 o número de
-- iterações recebe um reset, ou seja passa a ter o número inicial de iterações. Caso
-- delta seja maior que 0 o número de iterações é decrementado em 1
incr :: Int -> Float -> Int -> Int
incr it delta itInit
    | delta<0 = itInit
    | otherwise = it-1

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

-- Dado o caminho, o delta, c e n1, caso delta maior que 0 não muda o caminho
-- caso delta menor que 0 troca o ponto na posição c pelo ponto na posição n1
swap :: [Int] -> Float -> (Int,Int) -> [Int]
swap [] _ _  = []
swap route delta (c,n1)
    | delta<0 = routeSwaped
    | otherwise = route
    where
        temp = take c route ++ [route !! n1] ++ drop (c+1) route
        routeSwaped = take n1 temp ++ [route !! c] ++ drop (n1+1) temp

-- Dado o delta e o dist, caso delta menor que 0 adiciona o delta à distância
-- caso delta maior que 0 não atualiza o dist
updateDist :: Float -> Float -> Float
updateDist dist delta
    | delta<0 = dist + delta
    | otherwise = dist
