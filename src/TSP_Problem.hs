import Monte_Carlo(traveling_monte_carlo)
import Simulated_Annealing(simulatedAnnealing)
import System.Random
import Data.Time.Clock (diffUTCTime, getCurrentTime)

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

genPoints :: Int -> IO ([Point])
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

printResult :: (Float, [Int]) -> IO ()
printResult (d,p) = do
                        putStrLn ("Path found: " ++ show p)
                        putStrLn ("Total cost: " ++ show d)
                        putStrLn ""

timeFunc :: ([[Float]] -> Int -> IO (Float,[Int])) -> [[Float]] -> Int -> [Char] -> IO ()
timeFunc f graph it s = do
            start <- getCurrentTime
            result <- f graph it
            end <- getCurrentTime
            --printResult result
            putStrLn $ s ++ show (end `diffUTCTime` start) ++ " elapsed."

-- Run both methods to compare the solutions
parallelTravel :: Int -> Int -> Int -> IO ()
parallelTravel nodes it runs = do
                            graph <- genGraph nodes
                            sa <- sequence (replicate runs (timeFunc simulatedAnnealing graph it "SA: "))
                            mc <- sequence (replicate runs (timeFunc traveling_monte_carlo graph it "MC: "))
                            print sa
                            print mc

-- Main
main = do
    parallelTravel 10 100 4
