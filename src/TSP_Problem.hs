import Monte_Carlo
import Simulated_Annealing(simulatedAnnealing)
import System.Random

type Point = (Float, Float)

-- Função para debug da matriz
printMatrix :: [[Float]] -> IO ()
printMatrix [] = return ()
printMatrix (l:ls) = do 
                        print l
                        printMatrix ls

genGraph :: Int -> IO ([[Float]])
genGraph n = do
                nodes <- genPoints n
                let graph = map (graphNode nodes) [0..n-1]
                return graph

genPoints :: Int -> IO [Point]
genPoints 0 = return []
genPoints n = do
               x <- randomRIO(0.0, 10.0) 
               y <- randomRIO(0.0, 10.0) 
               rest <- genPoints (n-1)
               return ((x,y) : rest)

nodeDist :: Point -> Point -> Float
nodeDist (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

graphNode :: [Point] -> Int -> [Float]
graphNode nodes node = map (nodeDist (nodes !! node)) nodes


-- Testa o traveling usando o método de Monte Carlo
monte_carlo_test :: Int -> Int -> IO ()
monte_carlo_test it size = do
                matrix <- genGraph size
                (dist, path) <- traveling_monte_carlo matrix it
                print ("The path is: " ++ show path) 
                print ("The cost is: " ++ show dist)

-- Run test for the Simulated Annealing method
sa_test :: Int -> Int -> IO ()
sa_test it size = do
            graph <- genGraph size
            (dist, path) <- simulatedAnnealing graph it
            print ("Path found: " ++ show path)
            print ("Total cost: " ++ show dist)

printResults :: [(Float, [Int])] -> IO ()
printResults [] = putStrLn ""
printResults ((d,p) : t) = do
                            putStrLn ("Path found: " ++ show p)
                            putStrLn ("Total cost: " ++ show d)
                            putStrLn ""
                            printResults t
 

-- Run both methods to compare the solutions
parallelTravel :: Int -> Int -> Int -> IO ()
parallelTravel nodes it runs = do
                            graph <- genGraph nodes
                            sa <- sequence (replicate runs (simulatedAnnealing graph it))
                            putStrLn "Simulated Annealing"
                            printResults sa
                            mc <- sequence (replicate runs (traveling_monte_carlo graph it))
                            putStrLn "Monte Carlo"
                            printResults mc
               
-- Main
main :: IO ()
main = do
        print "TEST"
