module Monte_Carlo where

import System.Random
import Data.List

-- Recebe uma Matriz de floats com os custos entre os pontos (indices)
-- Devolve o caminho (lista de inteiros) e o custo total (float)
traveling_monte_carlo :: [[Float]] -> (Float,[Int])
traveling_monte_carlo [] = (0,[])
traveling_monte_carlo (hL:t) = (dist,route)  --iterations((hL:t),n,route,dist)
                            where
                                n = length hL
                                randValue = randomRIO (0,n)
                                route = (permutations [0..n]) !! randValue
                                pathLastToFirst = ((hL:t) !! last route) !! head route
                                dist = calcDist route (hL:t) pathLastToFirst

calcDist :: Num a => [Int] -> [[a]] -> a -> a
calcDist _ [] _ = 0
calcDist [] _ _ = 0
calcDist [a] ll dist = dist
calcDist (p:t) ll dist = calcDist t ll (dist+path)
                            where
                                path = (ll !! p) !! (head t)

iterations :: [[Float]] -> Int -> [Int] -> Float -> Int -> Int -> (Float,[Int])
iterations _ _ route dist 0 _ = (dist,route)
iterations ll n route dist it itStart = iterations ll n route dist it itStart
                            where
                                c = randomRIO (0,n)
                                (prev,n1,n2) = getNeighbors c n
                                delta = calcDelta (c,prev,n1,n2) route ll
                                route = swap route delta (c,n1)
                                dist = updateDist dist delta
                                it = incr it delta itStart

getNeighbors :: Int -> Int -> (Int,Int,Int)
getNeighbors 0 n = (n,1,2)
getNeighbors c n
        | c==n-1 = (n-2,n,0)
        | c==n = (n-1,0,1)
        | otherwise = (c-1,c+1,c+2)

incr :: Int -> Float -> Int -> Int
incr it delta itInit
    | delta<0 = itInit
    | otherwise = it-1

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
                                    delta = ll0 + ll1 - ll2 - ll3

swap :: [Int] -> Float -> (Int,Int) -> [Int]
swap [] _ _  = []
swap route delta (c,n1)
    | delta<0 = routeSwaped
    | otherwise = route
    where
        temp = take c route ++ [route !! n1] ++ drop (c+1) route
        routeSwaped = take n1 temp ++ [route !! c] ++ drop (n1+1) temp

updateDist :: Float -> Float -> Float
updateDist dist delta
    | delta<0 = dist + delta
    | otherwise = dist
