import Monte_Carlo
import Monte_CarloSA
import Simulated_Annealing
import System.Random

size :: Int
size = 4 

-- Gera cada linha da parte inferior da matriz simétrica
genLine :: [Float] -> Int -> IO ([Float])
genLine l 0 = return l
genLine l 1 = return (l ++ [0])
genLine l n = do
            v <- randomIO
            line <- genLine (v:l) (n-1)
            return line

-- Gera a parte inferior e a diagonal da matriz simétrica
genHalfMatrix :: [[Float]] -> Int -> IO ([[Float]]) 
genHalfMatrix l 0 = return l
genHalfMatrix l n = do
                line <- genLine [] n 
                halfMatrix <- genHalfMatrix (line:l) (n-1)
                return halfMatrix

-- Recebe "metade" de uma matriz (parte inferior) e transpõe
-- criando uma matriz "completa"
transpose :: [[Float]] -> Int -> Int -> IO ([[Float]])
transpose l 0 nI = return l
transpose (l:ls) n nI = do
                    let aux = map (!! (nI-n)) ls
                    let newL = l ++ aux
                    newLs <- transpose ls (n-1) nI
                    return (newL:newLs)

-- Gera uma matriz simétrica em que a diagonal tem tudo a 0
genMatrix :: [[Float]] -> Int -> IO ([[Float]])
genMatrix l 0 = return l
genMatrix l n = do
                halfMatrix <- genHalfMatrix [] n
                matrix <- transpose halfMatrix n n
                return matrix

-- Função para debug da matriz
printMatrix :: [[Float]] -> IO ()
printMatrix [] = return ()
printMatrix (l:ls) = do 
                        print l
                        printMatrix ls

-- Testa o traveling usando o método de Monte Carlo
monte_carlo_test :: IO ()
monte_carlo_test = do
                matrix <- genMatrix [] size
                (dist, path) <- traveling_monte_carlo matrix
                print ("The path is: " ++ show path) 
                print ("The cost is: " ++ show dist)

monte_carloSA_test :: IO ()
monte_carloSA_test = do
                matrix <- genMatrix [] size
                (dist, path) <- traveling_monte_carloSA matrix
                print ("The path is: " ++ show path) 
                print ("The cost is: " ++ show dist)

sa_test :: IO ()
sa_test = do
            dist_graph <- genMatrix [] size
            (dist, path) <- simulatedAnnealing dist_graph
            print ("Path found: " ++ show path)
            print ("Total cost: " ++ show dist)

-- Main
main :: IO ()
main = do
        print "TEST"
